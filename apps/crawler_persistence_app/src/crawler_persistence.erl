-module(crawler_persistence).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([add_index/2, get_index/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

add_index(Word, UrlId) ->
	dispatch_server:dispatch_add_index(Word, UrlId).

get_index(Word) ->
	data_api:get_index(Word).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


		
