-module(data_api).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([get_index/1, get_count/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

get_index(Word) ->
	case get_word_id(Word) of
		no_word ->
			[];
		
		WordId ->
			get_url_id_list(WordId)
	end.


get_count(words_coll) ->
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(conn_manager_server_master, words),
	db_helper:perform_action({count, {}}, ConnCfg);

get_count(index_coll) ->
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(conn_manager_server_master, index),
	db_helper:perform_action({count, {}}, ConnCfg);

get_count(indexes) ->
	ProjectionDoc = {'_id', 0, urls, 1},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(conn_manager_server_master, index),
	{ok, Cursor} = db_helper:perform_action({find, {}, ProjectionDoc}, ConnCfg),
	case mongo:rest(Cursor) of
		[] ->
			{ok, 0};
		
		ListOfUrlIdList ->
			Sum =lists:foldl(fun(X, Sum) ->
								{urls, UrlIdList} = X,
								Sum + length(UrlIdList)
						end, 0, ListOfUrlIdList),
			{ok, Sum}
	end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


		
%%
%% Bucket id retrieving helper functions.
%%
get_word_id(Word) ->
	SelectorDoc = {word, bson:utf8(Word)},
	ProjectionDoc = {'_id', 1},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(conn_manager_server_master, words),
	case db_helper:perform_action({find_one, SelectorDoc, ProjectionDoc}, ConnCfg) of
		{ok, {{'_id', WordId}}} ->
			WordId;
		
		{ok, {}} ->
			no_word
	end.

%%%
%%% Index retrieving helper functions.
%%%
get_url_id_list(WordId) ->
	SelectorDoc = {'_id', WordId}, 
	ProjectionDoc = {'_id', 0, urls, 1},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(conn_manager_server_master, index),
	{ok, {{urls, UrlIdList}}} = db_helper:perform_action({find_one, SelectorDoc, ProjectionDoc}, ConnCfg),
	UrlIdList.		
	