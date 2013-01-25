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

start_link([SupervisorName, ChannelId, ConnManagerServerName]) ->
    supervisor:start_link({local, SupervisorName}, ?MODULE, [ChannelId, ConnManagerServerName]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([ChannelId, ConnManagerServerName]) ->
	
	%% Set notification server.
	NotificationServerName = config_helper:get_worker_name(notification_server, ChannelId),
	NotificationServerSpec = ?CHILD_CHANNEL(NotificationServerName, notification_server, worker, 
											NotificationServerName, infinity),
	
	%% Set words cache server.
	WordsCacheServerCfg = config_helper:get_server_config(words_cache_server),
	WordsCacheServerName = config_helper:get_worker_name(words_cache_server, ChannelId),
	WordsCacheServerSpec = ?CHILD_CHANNEL(WordsCacheServerName, words_cache_server, worker, 
										  [WordsCacheServerName, WordsCacheServerCfg], infinity),
	
	%% Set persistence server.
	PersistenceServerName = config_helper:get_worker_name(persistence_server, ChannelId),
	HelperServersCfg = {{words_cache_server_name, WordsCacheServerName}, {notification_server_name, NotificationServerName},
				 {conn_manager_server_name, ConnManagerServerName}},
	PersistenceServerSpec = ?CHILD_CHANNEL(PersistenceServerName, persistence_server, worker,
										   [PersistenceServerName, HelperServersCfg], infinity),
	
    ChildrenSpecs = [NotificationServerSpec, WordsCacheServerSpec, PersistenceServerSpec],
    RestartStrategy = { one_for_one , 0, 1},
	
    {ok, { RestartStrategy, ChildrenSpecs } }. 


	

