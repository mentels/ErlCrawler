-module(config_helper).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([get_config/1, set_indexes/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

get_config(ServerName) ->
	get_config_internal(ServerName).

set_indexes() ->
	set_indexes_internal().

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_config_internal(persistence_server) ->
	{ok, PersistenceCfg} = application:get_env(persistence_cfg),
	PersistenceCfg;

get_config_internal(id_server) ->
	{ok, IdCfg}  = application:get_env(id_cfg),
	case get_max_word_id_and_bucket_id() of
		{no_id, no_id} ->
			IdCfg;
		
		{WordId, no_id} ->
			lists:keyreplace(init_word_id, 1, IdCfg, {init_word_id, WordId + 1});
		
		{no_id, BucketId} ->
			lists:keyreplace(init_bucket_id, 1, IdCfg, {init_bucket_id, BucketId + 1});
		
		{WordId, BucketId} ->
			[{init_word_id, WordId + 1}, {init_bucket_id, BucketId + 1}]
	
	end;

get_config_internal(worddb_server) ->
	{ok, WordDbCfg} = application:get_env(word_db_cfg),
	WordDbCfg;

get_config_internal(indexdb_server) ->
	{ok, IndexDbCfg} = application:get_env(index_db_cfg),
	IndexDbCfg;

get_config_internal(db_cleaner_server) ->
	{ok, DbCleanerCfg} = application:get_env(db_cleaner_cfg),
	DbCleanerCfg;

get_config_internal(cache_server) ->
	{ok, CacheServerCfg} = application:get_env(cache_cfg),
	CacheServerCfg;

get_config_internal(conn_manager_server) ->
	{ok, ConnManagerCfg} = application:get_env(conn_manager_cfg),
	ConnManagerCfg;

get_config_internal(_Other) ->
	undefined.


set_indexes_internal() ->
	case get_max_word_id() of
		no_id ->
			create_index_on_words_coll();
		
		_ ->
			ok
	end.
		
%%
%% Max id retrieving helper functions.
%%

get_max_word_id_and_bucket_id() ->
	{get_max_word_id(), get_max_bucket_id()}.
		  
		  
get_max_word_id() ->
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(words),
	get_max_id_from_db(ConnCfg).


get_max_bucket_id() ->
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(index),
	get_max_id_from_db(ConnCfg).
	

get_max_id_from_db(ConnCfg) ->
	SelectorDoc = {},
	ProjectionDoc = {'_id', 1},
	{ok, Cursor} = db_helper:perform_action({find, SelectorDoc, ProjectionDoc}, ConnCfg),
	case mongo:rest(Cursor) of
		[] ->
			no_id;
		
		IdList ->
			[{'_id' , MaxId} | _T ] = lists:sort(get_id_ordering_fun(), IdList),
			MaxId
	end.


get_id_ordering_fun() ->
	Fun = fun({'_id', IdA}, {'_id', IdB}) ->
		if
			IdA > IdB ->
				true;
			true ->
				false
		end
	end,
	Fun.

%%
%% Indexes creating helper functions.
%%
create_index_on_words_coll() ->
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(words),
	IndexSpec = {key, {word, 1}, unique, true},
	db_helper:perform_action({create_index, IndexSpec}, ConnCfg).