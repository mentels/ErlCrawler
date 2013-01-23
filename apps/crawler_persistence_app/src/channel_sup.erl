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
	
	%% Set notification server.
	NotificationServerName = config_helper:get_worker_name(notification_server, ChannelId),
	NotificationServerSpec = ?CHILD_CHANNEL(NotificationServerName, notification_server, worker, 
											NotificationServerName, infinity),
	
	%% Set db cleaner server.
	DbCleanerCfg = config_helper:get_server_config(db_cleaner_server),
	DbCleanerServerName = config_helper:get_worker_name(db_cleaner_server, ChannelId),
	DbCleanerServerSpec = ?CHILD_CHANNEL(DbCleanerServerName, db_cleaner_server, worker, 
										 [DbCleanerServerName, DbCleanerCfg], infinity),
	
	%% Set index cache server.
	CacheServerCfg = config_helper:get_server_config(cache_server),
	CacheServerName = config_helper:get_worker_name(cache_server, ChannelId),
	CacheServerSpec = ?CHILD_CHANNEL(CacheServerName, cache_server, worker, 
									 [CacheServerName, CacheServerCfg], infinity),
	
	%% Set words cache server.
	WordsCacheServerCfg = config_helper:get_server_config(words_cache_server),
	WordsCacheServerName = config_helper:get_worker_name(words_cache_server, ChannelId),
	WordsCacheServerSpec = ?CHILD_CHANNEL(WordsCacheServerName, words_cache_server, worker, 
										  [WordsCacheServerName, WordsCacheServerCfg], infinity),
	
	%% Set persistence server.
	PersistenceCfg = config_helper:get_server_config(persistence_server),
	PersistenceServerName = config_helper:get_worker_name(persistence_server, ChannelId),
	HelperServersCfg = {{words_cache_server_name, WordsCacheServerName},{index_cache_server_name, CacheServerName}, 
				 {cleaner_cache_server_name, DbCleanerServerName}, {notification_server_name, NotificationServerName}},
	PersistenceServerSpec = ?CHILD_CHANNEL(PersistenceServerName, persistence_server, worker,
										   [PersistenceServerName, HelperServersCfg, PersistenceCfg], infinity),
	
    ChildrenSpecs = [NotificationServerSpec, DbCleanerServerSpec, CacheServerSpec, WordsCacheServerSpec, PersistenceServerSpec],
    RestartStrategy = { one_for_one , 0, 1},
	
    {ok, { RestartStrategy, ChildrenSpecs } }. 


	

