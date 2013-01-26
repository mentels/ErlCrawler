%% @author maciek
%% @doc @todo Add description to processing_handler.


-module(processing_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([process_data/3,save_url/3, tokenize/1]).


process_data(Url_id, Url, Source)->
	try
		HTMLTree = mochiweb_html:parse(Source),
		Words = handle_words(HTMLTree),
		lager:log(notice,self(),string:concat(string:concat(Url, " "),erlang:integer_to_list(erlang:length(Words)))),
		FilterEtsName = list_to_atom(pid_to_list(self())),
		FilterEtsId = ets:new(FilterEtsName, [set, {keypos, 1}, private, named_table]),
		save_url(Words, Url_id, FilterEtsId),
		stats_server:add_page_stats(erlang:byte_size(Source),erlang:length(Words))
	catch
		_:_ -> []
	end.
	
save_url([],_, _)->
	ok;
save_url([H|T],Url_id, FilterEtsId) when length(H) > 1->
	%% Jesli slowa nie ma na stopliscie a jest w slowniku, to zapisujemy
	Word = string:to_lower(string:strip(H, both)),
	case stoplist_server:check_word(Word) of
		true ->
			case ets:insert_new(FilterEtsId, {Word}) of
				true ->
					%% slowa nie bylo na liscie filtrujacej - wpuszczamy do przetwarzania
					crawler_persistence:add_index(Word,Url_id);
				false ->
					%% slowo bylo na liscie filtrujacej, nie robimy nic
					ok
			end;
		
		_ -> 
			ok
	end,
	save_url(T,Url_id, FilterEtsId);
save_url([_H|T],Url_id, FilterEtsId) ->
	save_url(T,Url_id, FilterEtsId).

%% Wyciaga slowa i zleca ich zapisanie do bazy
handle_words(HTMLTree) ->
	Text = extractor:get_text(HTMLTree, <<>>),
	Words = tokenize(Text).

tokenize(Text) when is_binary(Text) ->
	string:tokens(erlang:binary_to_list(Text), ";:'[],./|?!  \"\n+-=");
tokenize(Text) ->
	string:tokens(unicode:characters_to_list(Text,utf8), ";:'[],./|?!  \"\n+-=").
