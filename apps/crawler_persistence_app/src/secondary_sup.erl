-module(secondary_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args, ShutdownTimeout), {I, {I, start_link, [Args]}, permanent, ShutdownTimeout, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_StartArgs) ->
	
	config_helper:set_indexes(),

	%% Set id server.
	IdCfg = config_helper:get_config(id_server),
	IdServerSpec = ?CHILD(id_server, worker, IdCfg, brutal_kill),
	
	%% Set db cleaner server.
	DbCleanerCfg = config_helper:get_config(db_cleaner_server),
	DbCleanerServerSpec = ?CHILD(db_cleaner_server, worker, DbCleanerCfg, infinity),
	
	%% Set cache server.
	CacheServerCfg = config_helper:get_config(cache_server),
	CacheServerSpec = ?CHILD(cache_server, worker, CacheServerCfg, infinity),
	
	%% Set persistence server.
	PersistenceCfg = config_helper:get_config(persistence_server),
	PersistenceServerSpec = ?CHILD(persistence_server, worker, PersistenceCfg, infinity),
	
    ChildrenSpecs = [ IdServerSpec, DbCleanerServerSpec, CacheServerSpec, PersistenceServerSpec],
    RestartStrategy = { one_for_one , 0, 1},
	
    {ok, { RestartStrategy, ChildrenSpecs } }. 


	
	

