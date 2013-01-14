-module(persistence_configurator).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([get_config/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

get_config(ServerName) ->
	get_config_internal(ServerName).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_config_internal(persistence_server) ->
	{ok, PersistenceCfg} = application:get_env(crawler, persistence_cfg),
	PersistenceCfg;

get_config_internal(id_server) ->
	MaxWordId = get_max_word_id_from_db(),
%% 	MaxBucketId = get_max_bucket_id_from_db(),
%% 	{MaxWordId, MaxBucketId}.
	MaxWordId.
			

get_max_word_id_from_db() ->
	{ok, WordDbCfg} = application:get_env(crawler, word_db_cfg),
	[
  		{conn_cfg, ConnCfg},
  		{db, DbName},
		{coll, CollName}
  	] = WordDbCfg,
	{ok, Conn} = mongo:connect(ConnCfg),
	SelectorDoc = {},
	ProjectionDoc = {'_id', 1},
	{ok, Cursor} = db_helper:perform_action({find, SelectorDoc, ProjectionDoc}, CollName, DbName, Conn),
	lists:sort(get_id_ordering_fun(), mongo:rest(Cursor)).
	

get_id_ordering_fun() ->
	Fun = fun({'_id', IdA}, {'_id', IdB}) ->
		if
			IdA =< IdB ->
				true;
			true ->
				false
		end
	end,
	Fun.

	