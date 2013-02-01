-module(channel_primary_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args, ShutdownTimeout), {I, {I, start_link, [Args]}, permanent, ShutdownTimeout, Type, [I]}).

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
	
	%% Set connectin manager.
	ConnManagerCfg = config_helper:get_server_config(conn_manager_server),
	ConnManagerName = config_helper:get_worker_name(conn_manager_server, ChannelId),
	ConnManagerServerSpec = ?CHILD_CHANNEL(ConnManagerName, conn_manager_server, worker, 
										   [ConnManagerName, ConnManagerCfg], infinity),
	
	%% Set words cache server.
	WordsCacheServerCfg = config_helper:get_server_config(words_cache_server),
	WordsCacheServerName = config_helper:get_worker_name(words_cache_server, ChannelId),
	WordsCacheServerSpec = ?CHILD_CHANNEL(WordsCacheServerName, words_cache_server, worker, 
										  [WordsCacheServerName, WordsCacheServerCfg], infinity),
	

	%% Set persistence server.
	PersistenceCfg = config_helper:get_server_config(persistence_server),
	PersistenceServerName = config_helper:get_worker_name(persistence_server, ChannelId),
	HelperServersCfg = {{words_cache_server_name, WordsCacheServerName}, {conn_manager_server_name, ConnManagerName}},
	PersistenceServerSpec = ?CHILD_CHANNEL(PersistenceServerName, persistence_server, worker,
										   [PersistenceServerName, HelperServersCfg, PersistenceCfg], infinity),

    {ok, { { one_for_one , 0, 1}, [ConnManagerServerSpec, WordsCacheServerSpec, PersistenceServerSpec] } }. 
