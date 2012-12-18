
-module(crawler_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [StartArgs]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_StartArgs) ->
	{ok, [{max_workers,Max_workers},{uds_fail_interval,Uds_fail_interval},{connection_timeout,Connection_timeout},{download_timeout,Download_timeout},{redirect_limit,Redirect_limit}]} = application:get_env(processing_cfg),
	Processing_server_spec = ?CHILD(download_manager,worker,{Max_workers,Uds_fail_interval,Connection_timeout,Download_timeout,Redirect_limit}),
 	ChildrenSpecs = [Processing_server_spec],
    RestartStrategy = { one_for_one , 5, 10},
    {ok, {RestartStrategy, ChildrenSpecs}}.

