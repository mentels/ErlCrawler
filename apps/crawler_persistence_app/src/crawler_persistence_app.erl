-module(crawler_persistence_app).

-behaviour(application).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, StartArgs) ->
	lager:start(),
	application:start(mongodb),
	application:start(bson),
    primary_sup:start_link(StartArgs).

prep_stop(_) ->
    persistence_server:prepare_to_stop().

stop(_State) ->
	ok.