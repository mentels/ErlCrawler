-module(crawler_sup).

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
	%% Set persistence server.
	{ok, PersistenceCfg} = application:get_env(persistence_cfg),
	PersistenceServerSpec = ?CHILD(persistence_server, worker, PersistenceCfg, 2000),
	
	%% Set id server.
	{ok, IdCfg} = application:get_env(id_cfg),
	IdServerSpec = ?CHILD(id_server, worker, IdCfg, 2000),
	
	%% Set word db server.
	{ok, WordDbCfg} = application:get_env(word_db_cfg),
	WordsDbServerSpec = ?CHILD(worddb_server, worker, WordDbCfg, 2000),
	
	%% Set index db server.
	{ok, IndexDbCfg} = application:get_env(index_db_cfg),
	IndexDbServerSpec = ?CHILD(indexdb_server, worker, IndexDbCfg, 2000),

	%% Set db cleaner server.
	{ok, DbCleanerCfg} = application:get_env(db_cleaner_cfg),
	DbCleanerServerSpec = ?CHILD(db_cleaner_server, worker, DbCleanerCfg, 2000),
	
	%% Set cache server.
	{ok, CacheServerCfg} = application:get_env(cache_cfg),
	CacheServerSpec = ?CHILD(cache_server, worker, CacheServerCfg, 2000),
	
    ChildrenSpecs = [ PersistenceServerSpec, IdServerSpec, WordsDbServerSpec, IndexDbServerSpec, DbCleanerServerSpec, CacheServerSpec ],
    RestartStrategy = { one_for_one , 0, 1},
	
    {ok, { RestartStrategy, ChildrenSpecs } }. 

	
	
	

