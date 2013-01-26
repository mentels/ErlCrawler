-module(crawler_config_controller, [Req]).
-compile(export_all).

stats('GET',[]) ->
 	{ok,Nodes} = application:get_env(crawler,nodes),
	{ok, [{stats,lists:map(fun(X) -> {X,utils:node_stats(X)} end , Nodes)}]}.
 
configs('GET',[]) ->
 	ok.