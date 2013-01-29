-module(crawler_search_controller, [Req]).
-compile(export_all).

index('GET', []) ->
    {ok,Nodes} = application:get_env(crawler,nodes),
	Active_nodes = utils:ping_nodes(Nodes),
    {ok,[{nodes,Active_nodes}]}.

results('POST',[])->
     Arg = Req:post_param("word"),
 	{ok,Nodes} = application:get_env(crawler,nodes),
	Active_nodes = utils:ping_nodes(Nodes),
 	Urls = utils:query_nodes_4_word(Active_nodes,Arg,[]),
    {ok, [{urls,Urls}]}.
