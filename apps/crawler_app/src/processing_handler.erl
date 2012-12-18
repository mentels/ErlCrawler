%% @author maciek
%% @doc @todo Add description to processing_handler.


-module(processing_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([process_data/3]).


process_data(Url_id, Url, Source)->
	try
		HTMLTree = mochiweb_html:parse(Source),
		extractor:get_http_urls(Url, HTMLTree)
	catch
		_ -> []
	end.
	
	

%% Wyciaga slowa i zleca ich zapisanie do bazy
handle_words(HTMLTree) ->
	Text = extractor:get_text(HTMLTree, <<>>),
	Words = string:tokens(unicode:characters_to_list(Text,utf8), ";:'[],./|?!  \"\n+-="),
	%% TODO : Obslużyć persystencję słów
	ok.
