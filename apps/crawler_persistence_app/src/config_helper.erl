-module(config_helper).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([get_server_config/1, get_channels_config/0, set_indexes/0, get_worker_name/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

get_server_config(ServerName) ->
	get_server_config_internal(ServerName).


get_channels_config() ->
	get_channels_config_internal().


set_indexes() ->
	set_indexes_internal().


get_worker_name(WorkerModule, ChannelId) ->
	list_to_atom(atom_to_list(WorkerModule) ++ "_ch" ++ integer_to_list(ChannelId)). 


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_server_config_internal(persistence_server) ->
	{ok, PersistenceCfg} = application:get_env(persistence_cfg),
	PersistenceCfg;

get_server_config_internal(id_server) ->
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

get_server_config_internal(db_cleaner_server) ->
	{ok, DbCleanerCfg} = application:get_env(db_cleaner_cfg),
	DbCleanerCfg;

get_server_config_internal(index_cache_server) ->
	{ok, CacheServerCfg} = application:get_env(index_cache_cfg),
	CacheServerCfg;

get_server_config_internal(conn_manager_server) ->
	{ok, ConnManagerCfg} = application:get_env(conn_manager_cfg),
	ConnManagerCfg;

get_server_config_internal(words_cache_server) ->
	{ok, WordsCacheCfg} = application:get_env(words_cache_cfg),
	WordsCacheCfg;

get_server_config_internal(_Other) ->
	undefined.


get_channels_config_internal() ->
	{ok, ChannelsCfg} = application:get_env(channels_cfg),
	ChannelsCfg.


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