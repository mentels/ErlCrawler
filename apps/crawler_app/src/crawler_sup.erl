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
	{ok, InitialID} = application:get_env(initial_id),
	IdServerSpec = ?CHILD(id_server, worker, InitialID, 2000),
	lager:info("Id server started."),
	
	%% Set Words and Index Db servers.
	{ok, DbCfg} = application:get_env(db_cfg),
	WordsDbServerSpec = ?CHILD(wordsdb_server, worker, DbCfg, 2000),
	IndexDbServerSpec = ?CHILD(indexdb_server, worker, DbCfg, 2000),
	lager:info("WordsDb and IndexDb servers started."),

	%% Set db cleaner server.
	{ok, DbCleanerCfg} = application:get_env(db_cleaner_cfg),
	DbCleanerServerSpec = ?CHILD(db_cleaner_server, worker, DbCleanerCfg, 2000),
	lager:info("Cleaner server started."),
	
	%% Set cache server.
	{ok, CacheServerCfg} = application:get_env(cache_cfg),
	CacheServerSpec = ?CHILD(cache_server, worker, CacheServerCfg, 2000),
	lager:info("Cache server started."),
	
    ChildrenSpecs = [ PersistenceServerSpec, IdServerSpec, WordsDbServerSpec, IndexDbServerSpec, DbCleanerServerSpec, CacheServerSpec ],
    RestartStrategy = { one_for_one , 0, 1},
	lager:critical("Test critital log"),
	lager:debug("Test debug log"),
	lager:notice("Test notice log"),
	
    {ok, { RestartStrategy, ChildrenSpecs } }. 

	
	
	

