%% @author maciek
%% @doc @todo Add description to processing_handler.


-module(processing_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([process_data/3,save_url/2, tokenize/1]).


process_data(Url_id, Url, Source)->
	try
		HTMLTree = mochiweb_html:parse(Source),
		Words = handle_words(HTMLTree),
		lager:log(notice,self(),string:concat(string:concat(Url, " "),erlang:integer_to_list(erlang:length(Words)))),
		save_url(Words, Url_id)
	catch
		_ -> []
	end.
	
save_url([],_)->
	ok;
save_url([H|T],Url_id) ->
	%% Jesli slowa nie ma na stopliscie, to zapisujemy
	case stoplist_server:check_word(H) of
		false -> persistence_server:add_index(H,Url_id);
		_ -> ok
	end,
	save_url(T,Url_id).

%% Wyciaga slowa i zleca ich zapisanie do bazy
handle_words(HTMLTree) ->
	Text = extractor:get_text(HTMLTree, <<>>),
	Words = tokenize(Text).

tokenize(Text) when is_binary(Text) ->
	string:tokens(erlang:binary_to_list(Text), ";:'[],./|?!  \"\n+-=");
tokenize(Text) ->
	string:tokens(unicode:characters_to_list(Text,utf8), ";:'[],./|?!  \"\n+-=").