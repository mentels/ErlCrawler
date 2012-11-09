-module(crawler_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, StartArgs) ->
	lager:start(),
    crawler_sup:start_link(StartArgs).

stop(_State) ->
    ok.

