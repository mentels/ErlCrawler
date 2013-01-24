-module(crawler_persistence_sup).

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

start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_StartArgs) ->
	
	%% Set connectin manager.
	ConnectionManagerCfg = config_helper:get_server_config(conn_manager_server),
	ConnManagerServerSpec = ?CHILD_CHANNEL(conn_manager_server_master, conn_manager_server, 
										   worker, ConnectionManagerCfg, infinity),
	
	%% Set id and dispatch servers.
	IdAndDispatchServersSup = ?CHILD(id_and_dispatch_servers_sup, supervisor, [], infinity),
	
	%% Set channels supervisors.
	[{channels_cnt, ChannelCnt}] = config_helper:get_channels_config(),
	ChannelPrimarySupSpecs = get_channel_supervisor_specs(ChannelCnt, []),
	
    ChildrenSpecs = [ ConnManagerServerSpec, IdAndDispatchServersSup | ChannelPrimarySupSpecs],
    RestartStrategy = { one_for_one , 0, 1},
	
    {ok, { RestartStrategy, ChildrenSpecs } }. 



get_channel_supervisor_specs(ChannelsCnt, ChannelSupSpecs) when ChannelsCnt > 0 ->
	ChannelSupName = config_helper:get_worker_name(channel_primary_sup, ChannelsCnt),
	ChannelId = ChannelsCnt,
	ChannelSupSpec = ?CHILD_CHANNEL(ChannelSupName, channel_primary_sup, supervisor, 
									[ChannelSupName, ChannelId], infinity),
	get_channel_supervisor_specs(ChannelsCnt - 1, [ChannelSupSpec | ChannelSupSpecs]);

get_channel_supervisor_specs(ChannelsCnt, ChannelSupSpecs) when ChannelsCnt == 0 ->
	ChannelSupSpecs.
	
	
	

