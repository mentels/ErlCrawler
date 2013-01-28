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

	
	%% Set secondary channel supervisor
	ChannelSecondarySupName = config_helper:get_worker_name(channel_secondary_sup, ChannelId),
	ChannelSecondarySupSpec = ?CHILD_CHANNEL(ChannelSecondarySupName, channel_secondary_sup, supervisor, 
										 [ChannelSecondarySupName, ChannelId, ConnManagerName], infinity),
	
	
    {ok, { { one_for_one , 0, 1}, [ChannelSecondarySupSpec] } }. 
