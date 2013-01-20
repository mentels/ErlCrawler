-module(primary_sup).

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
	
	%% Set connectin manager.
	ConnectionManagerCfg = config_helper:get_config(conn_manager_server),
	ConnManagerServerSpec = ?CHILD(conn_manager_server, worker, ConnectionManagerCfg, infinity),
	
	SecondarySup = ?CHILD(secondary_sup, supervisor, [], infinity),
	
    ChildrenSpecs = [ ConnManagerServerSpec, SecondarySup],
    RestartStrategy = { one_for_one , 0, 1},
	
    {ok, { RestartStrategy, ChildrenSpecs } }. 


	
	

