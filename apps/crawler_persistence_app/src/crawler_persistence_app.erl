-module(crawler_persistence_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, StartArgs) ->
	lager:start(),
	application:start(mongodb),
	application:start(bson),
    crawler_persistence_sup:start_link(StartArgs).

stop(_State) ->
    ok.
