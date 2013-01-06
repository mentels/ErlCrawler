%% @author maciek
%% @doc @todo Add description to extractor.


-module(extractor).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_urls/2,get_text/2,get_words/1,extract_href/1,get_http_urls/2,check_address/1,simp_rel2/3, simp_rel1/3, simplify_urls/3]).


%% processing_handler_test:test().

get_http_urls(Address,Content) ->
	Urls = get_urls(Content,[]),
	Proper_address = check_address(Address),
	simplify_urls(Proper_address,Urls,[]).

simplify_urls(Address,[H|T],Accum) ->
	case string:to_lower(string:left(H, 7)) of
		"http://" -> 
				simplify_urls(Address,T,Accum++[H]);
		"mailto:" ->
				simplify_urls(Address,T,Accum);
		_ -> simp_rel2(Address,[H|T],Accum)
			
	end;

	

simplify_urls(Address,[],Accum)->
	Accum.

simp_rel2(Address,[H|T],Accum) ->
	case string:left(H,2) of
		%%Relative links. We have to cut the 'http://' heading because mochiweb cant deal with such paths. 
		".." -> case mochiweb_util:safe_relative_path(string:concat(string:sub_string(Address, 8),H)) of
					undefined -> simplify_urls(Address,T,Accum);
					Url -> simplify_urls(Address,T,Accum++[string:concat("http://",Url)]);
					_ -> simplify_urls(Address,T,Accum)
				end;
		"./" -> case mochiweb_util:safe_relative_path(string:concat(string:sub_string(Address, 8),string:sub_string(H, 3))) of
					undefined -> simplify_urls(Address,T,Accum);
					Url -> simplify_urls(Address,T,Accum++[string:concat("http://",Url)]);
					_ -> simplify_urls(Address,T,Accum)
				end;
		_ -> simp_rel1(Address,[H|T],Accum)			
	end.

simp_rel1(Address,[H|T],Accum) ->
	case string:left(H,1) of
		%%Cutting leading /, because http_url contains this
		"/" -> case mochiweb_util:safe_relative_path(string:concat(string:sub_string(Address, 8),string:sub_string(H,2))) of
				   undefined -> simplify_urls(Address,T,Accum);
				   Url -> simplify_urls(Address,T,Accum++[string:concat("http://",Url)]);
					_ -> simplify_urls(Address,T,Accum)
			   end;
		_ -> simplify_urls(Address,T,Accum)			
	end.

check_address(Address) ->
	case string:right(Address, 1) of 
		"/" -> Address;
		_ -> string:concat(Address, "/")	
	end.
			

get_urls(Content,Accum) when is_bitstring(Content) ->
	Accum;
get_urls([Content],Accum) when is_bitstring(Content) ->
	Accum;
get_urls({comment,_},Accum)-> %%Komentarze pomijamy
	Accum;
get_urls([{comment,_}],Accum)-> %%Komentarze pomijamy
	Accum;
get_urls([H | T],Accum)->
	NewAccum = get_urls(H,Accum),
	get_urls(T,NewAccum);
get_urls([],Accum)->
	Accum;
get_urls({<<"a">>,Attributes,Childs},Accum) ->
	Accum++extract_href(Attributes);
get_urls({<<"A">>,Attributes,Childs},Accum) ->
	Accum++extract_href(Attributes);	
get_urls({_Name,Attributes,Childs},Accum) ->
	get_urls(Childs,Accum).


extract_href([{Attr,Value}|T]) ->
	case string:to_lower(erlang:binary_to_list(Attr)) of
		  "href" -> [erlang:binary_to_list(Value)];
			_ -> extract_href(T)
	end;

extract_href([])->
	[].
				

get_words(Binary)->
	string:tokens(unicode:characters_to_list(Binary,utf8), ";:'[],./|?!  \"\n+-=").

get_text({comment,_},Accum)-> %%Komentarze pomijamy
	Accum;
get_text(Content,Accum) when is_bitstring(Content) -> %% Ostateczna tre.... do wydrukowania
	<<" ",Accum/binary," ",Content/binary>>;
get_text([Content],Accum) when is_bitstring(Content) -> %% Ostateczna tre.... do wydrukowania
	<<" ",Accum/binary," ",Content/binary>>;
get_text({<<"script">>,_,_},Accum)-> %% pomijamy skrypt
	Accum;
get_text({<<"style">>,_,_},Accum)-> %%pomijamy style
	Accum;
get_text({_NodeName,_,[Content]},Accum) when is_bitstring(Content) -> %%Wezel z trescia do wydrukowania - mozna uproscic ! Klauzula ponizej powinna to zalatwic
	<<" ",Accum/binary," ",Content/binary>>;
get_text({_NodeName,_,[Content]},Accum)-> %% Node  
	get_text(Content,Accum);
get_text({_NodeName,_,[Content|Tail]},Accum)-> % Node z lista dzieci (tez node'ow)
	New = get_text(Content,Accum),
	get_text(Tail,New);
get_text({_NodeName,_,[]},Accum)-> % Node z pusta trescia (np. Head)
	Accum;
get_text([Head|Tail],Accum)-> % Lista wezlow - dzieci
	New = get_text(Head,Accum),
	get_text(Tail,New);
get_text([],Accum) ->
	Accum;
get_text({},Accum) ->
	Accum.
