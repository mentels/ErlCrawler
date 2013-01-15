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
		io:format("~s ~w ",[Url,lists:flatlength(Words)]),
		save_url(Words, Url_id)
	catch
		_ -> []
	end.
	
save_url([],_)->
	ok;
save_url([H|T],Url_id) ->
	persistence_server:add_index(H,Url_id),
	save_url(T,Url_id).

%% Wyciaga slowa i zleca ich zapisanie do bazy
handle_words(HTMLTree) ->
	Text = extractor:get_text(HTMLTree, <<>>),
	Words = tokenize(Text).

tokenize(Text) when is_binary(Text) ->
	string:tokens(erlang:binary_to_list(Text), ";:'[],./|?!  \"\n+-=");
tokenize(Text) ->
	string:tokens(unicode:characters_to_list(Text,utf8), ";:'[],./|?!  \"\n+-=").