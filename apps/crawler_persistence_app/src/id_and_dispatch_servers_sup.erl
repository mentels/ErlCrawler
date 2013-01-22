-module(id_and_dispatch_servers_sup).

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
	IdCfg = config_helper:get_server_config(id_server),
	IdServerSpec = ?CHILD(id_server, worker, IdCfg, brutal_kill),
	
	% Set dispatcher server.
	[ChannelsCfg] = config_helper:get_channels_config(),
	{channels_cnt, ChannelsCnt} = ChannelsCfg,
	PersistenceServerNameList = get_persistence_server_names(ChannelsCnt, []),
	DispatcherCfg = {ChannelsCfg, {persistence_server_names, PersistenceServerNameList}},
	DispatchServerSpec = ?CHILD(dispatch_server, worker, DispatcherCfg, infinity),
	
    ChildrenSpecs = [ IdServerSpec, DispatchServerSpec],
    RestartStrategy = { one_for_one , 0, 1},
	
    {ok, { RestartStrategy, ChildrenSpecs } }. 


get_persistence_server_names(0, PersistenceServerNameList) ->
	PersistenceServerNameList;

get_persistence_server_names(ChannelsCnt, PersistenceServerNameList) ->
	PersistenceServerName = config_helper:get_worker_name(persistence_server, ChannelsCnt),
	get_persistence_server_names(ChannelsCnt - 1, [PersistenceServerName | PersistenceServerNameList]).

