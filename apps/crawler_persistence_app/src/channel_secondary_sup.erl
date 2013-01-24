-module(channel_secondary_sup).

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

start_link([SupervisorName, ChannelId, ChannelsCnt, ConnManagerServerName]) ->
    supervisor:start_link({local, SupervisorName}, ?MODULE, [ChannelId, ChannelsCnt, ConnManagerServerName]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([ChannelId, ChannelsCnt, ConnManagerServerName]) ->
	
	%% Set id server.
	IdCfg = config_helper:get_server_config(id_server),
	IdServerName = config_helper:get_worker_name(id_server, ChannelId),
	ChannelsCfg = {{channel_id, ChannelId}, {channels_cnt, ChannelsCnt}},
	IdServerSpec = ?CHILD(id_server, worker, [IdServerName, ChannelsCfg, IdCfg], infinity),
	
	%% Set notification server.
	NotificationServerName = config_helper:get_worker_name(notification_server, ChannelId),
	NotificationServerSpec = ?CHILD_CHANNEL(NotificationServerName, notification_server, worker, 
											NotificationServerName, infinity),
	
	%% Set db cleaner server.
	DbCleanerCfg = config_helper:get_server_config(db_cleaner_server),
	DbCleanerServerName = config_helper:get_worker_name(db_cleaner_server, ChannelId),
	DbCleanerServerSpec = ?CHILD_CHANNEL(DbCleanerServerName, db_cleaner_server, worker, 
										 [DbCleanerServerName, ConnManagerServerName, DbCleanerCfg], infinity),
	
	%% Set index cache server.
	IndexCacheServerCfg = config_helper:get_server_config(index_cache_server),
	IndexCacheServerName = config_helper:get_worker_name(index_cache_server, ChannelId),
	IndexCacheServerSpec = ?CHILD_CHANNEL(IndexCacheServerName, index_cache_server, worker, 
									 [IndexCacheServerName, IndexCacheServerCfg], infinity),
	
	%% Set words cache server.
	WordsCacheServerCfg = config_helper:get_server_config(words_cache_server),
	WordsCacheServerName = config_helper:get_worker_name(words_cache_server, ChannelId),
	WordsCacheServerSpec = ?CHILD_CHANNEL(WordsCacheServerName, words_cache_server, worker, 
										  [WordsCacheServerName, WordsCacheServerCfg], infinity),
	
	%% Set persistence server.
	PersistenceCfg = config_helper:get_server_config(persistence_server),
	PersistenceServerName = config_helper:get_worker_name(persistence_server, ChannelId),
	HelperServersCfg = {{words_cache_server_name, WordsCacheServerName},{index_cache_server_name, IndexCacheServerName}, 
				 {cleaner_cache_server_name, DbCleanerServerName}, {notification_server_name, NotificationServerName},
				 {id_server_name, IdServerName}, {conn_manager_server_name, ConnManagerServerName}},
	PersistenceServerSpec = ?CHILD_CHANNEL(PersistenceServerName, persistence_server, worker,
										   [PersistenceServerName, HelperServersCfg, PersistenceCfg], infinity),
	
    ChildrenSpecs = [NotificationServerSpec, DbCleanerServerSpec, IndexCacheServerSpec, WordsCacheServerSpec, PersistenceServerSpec],
    RestartStrategy = { one_for_one , 0, 1},
	
    {ok, { RestartStrategy, ChildrenSpecs } }. 


	

