-module(crawler_search_controller, [Req]).
-compile(export_all).

index('GET', []) ->
    {ok,Nodes} = application:get_env(crawler,nodes),
    {ok,[{nodes,Nodes}]}.

results('POST',[])->
     Arg = Req:post_param("word"),
 	{ok,Nodes} = application:get_env(crawler,nodes),
 	Urls = utils:query_nodes_4_word(Nodes,Arg,[]),
    {ok, [{urls,Urls}]}.
