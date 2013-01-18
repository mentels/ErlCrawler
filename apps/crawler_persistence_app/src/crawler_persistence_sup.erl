-module(crawler_persistence_sup).

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
	
	%% Set id server.
	IdCfg = config_helper:get_config(id_server),
	IdServerSpec = ?CHILD(id_server, worker, IdCfg, brutal_kill),
	
	%% Set word db server.
	WordDbCfg = config_helper:get_config(worddb_server),
	WordsDbServerSpec = ?CHILD(worddb_server, worker, WordDbCfg, 2000),
	
	%% Set index db server.
	IndexDbCfg = config_helper:get_config(indexdb_server),
	IndexDbServerSpec = ?CHILD(indexdb_server, worker, IndexDbCfg, 2000),

	%% Set db cleaner server.
	DbCleanerCfg = config_helper:get_config(db_cleaner_server),
	DbCleanerServerSpec = ?CHILD(db_cleaner_server, worker, DbCleanerCfg, 2000),
	
	%% Set cache server.
	CacheServerCfg = config_helper:get_config(cache_server),
	CacheServerSpec = ?CHILD(cache_server, worker, CacheServerCfg, 2000),
	
	%% Set persistence server.
	PersistenceCfg = config_helper:get_config(persistence_server),
	PersistenceServerSpec = ?CHILD(persistence_server, worker, PersistenceCfg, 2000),
	
    ChildrenSpecs = [ IdServerSpec, WordsDbServerSpec, IndexDbServerSpec, DbCleanerServerSpec, CacheServerSpec, PersistenceServerSpec ],
    RestartStrategy = { one_for_one , 0, 1},
	
    {ok, { RestartStrategy, ChildrenSpecs } }. 


	
	

