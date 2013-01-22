-module(channel_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(
	CHILD(I, Type, Args, ShutdownTimeout), 
		{I, 
		 {I, start_link, [Args]}, 
		 permanent, 
		 ShutdownTimeout, 
		 Type, 
		 [I]
	}).

%% Helper macro for declaring children of supervisor that are channel supervisors
-define(CHILD_CHANNEL(I, M, Type, Args, ShutdownTimeout), {I, {M, start_link, [Args]}, permanent, ShutdownTimeout, Type, [M]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link([SupervisorName, ChannelId]) ->
    supervisor:start_link({local, SupervisorName}, ?MODULE, ChannelId).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(ChannelId) ->
	
	%% Set db cleaner server.
	DbCleanerCfg = config_helper:get_server_config(db_cleaner_server),
	DbCleanerServerName = config_helper:get_worker_name(db_cleaner_server, ChannelId),
	DbCleanerServerSpec = ?CHILD_CHANNEL(DbCleanerServerName, db_cleaner_server, worker, [DbCleanerServerName, DbCleanerCfg], infinity),
	
	%% Set cache server.
	CacheServerCfg = config_helper:get_server_config(cache_server),
	CacheServerName = config_helper:get_worker_name(cache_server, ChannelId),
	CacheServerSpec = ?CHILD_CHANNEL(CacheServerName, cache_server, worker, [CacheServerName, CacheServerCfg], infinity),
	
	%% Set persistence server.
	PersistenceCfg = config_helper:get_server_config(persistence_server),
	PersistenceServerName = config_helper:get_worker_name(persistence_server, ChannelId),
	CachesCfg = {{index_cache_server_name, CacheServerName}, {cleaner_cache_server_name, DbCleanerServerName}},
	AggregatedCfg = [PersistenceServerName, CachesCfg, PersistenceCfg],
	PersistenceServerSpec = ?CHILD_CHANNEL(PersistenceServerName, persistence_server, worker, AggregatedCfg, infinity),
	
    ChildrenSpecs = [DbCleanerServerSpec, CacheServerSpec, PersistenceServerSpec],
    RestartStrategy = { one_for_one , 0, 1},
	
    {ok, { RestartStrategy, ChildrenSpecs } }. 


	

