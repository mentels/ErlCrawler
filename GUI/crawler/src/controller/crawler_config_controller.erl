-module(crawler_config_controller, [Req]).
-compile(export_all).

stats('GET',[]) ->
 	{ok,Nodes} = application:get_env(crawler,nodes),
	Active_nodes = utils:ping_nodes(Nodes),
	{ok, [{stats,lists:map(fun(X) -> {X,utils:node_stats(X)} end , Active_nodes)}]}.
 
configs('GET',[]) ->
	{ok,Nodes} = application:get_env(crawler,nodes),
	Active_nodes = utils:ping_nodes(Nodes),
	{ok, [{confs,lists:map(fun(X) -> {X,utils:get_down_man_conf(X)} end , Active_nodes)}]}.

save('POST',[])->
	Node = Req:post_param("node"),
	Max_Active_Workers = Req:post_param("max_active_workers"),
	Max_redir = Req:post_param("max_redir"),
	Down_timeout =Req:post_param("down_timeout"),
	Conn_timeout = Req:post_param("conn_timeout"),
	utils:set_down_man_conf(Node,{Max_Active_Workers,Max_redir,Down_timeout,Conn_timeout}),
	{redirect, "/config/configs"}.
