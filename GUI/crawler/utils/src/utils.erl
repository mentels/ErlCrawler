%% @author maciek
%% @doc @todo Add description to utils.


-module(utils).

%% ====================================================================
%% API functions
%% ====================================================================
-export([query_nodes/4,query_nodes_4_word/3,node_stats/1,get_down_man_conf/1,set_down_man_conf/2]).


set_down_man_conf(Node,{Max_Active_Workers,Max_redir,Down_timeout,Conn_timeout})->
	rpc:call(list_to_atom(Node),download_manager,set_max_active_workers,[list_to_integer(Max_Active_Workers)],1000),
	rpc:call(list_to_atom(Node),download_manager,set_max_redit,[list_to_integer(Max_redir)],1000),
	rpc:call(list_to_atom(Node),download_manager,set_down_timeout,[list_to_integer(Down_timeout)],1000),
	rpc:call(list_to_atom(Node),download_manager,set_conn_timeout,[list_to_integer(Conn_timeout)],1000),
	rpc:call(list_to_atom(Node),download_manager,start_processing,[],1000),
	ok.

get_down_man_conf(Node)->

	case rpc:call(Node,download_manager,get_max_active_workers,[],1000) of
		{badrpc,_} ->
			Res = [{max_active_workers,[]}];
		{Answer} ->
			Res =  	[{max_active_workers,Answer}];
		_ ->
			Res = [{max_active_workers,[]}]
	end,
	case rpc:call(Node,download_manager,get_current_active_workers,[],1000) of
		{badrpc,_} ->
			Res1 = Res ++ [{current_active_workers,[]}];
		{Answer1} ->
			Res1 = Res ++ [{current_active_workers,Answer1}];
		_ ->
			Res1 = Res ++ [{current_active_workers,[]}]
	end,
	case rpc:call(Node,download_manager,get_max_redir,[],1000) of
		{badrpc,_} ->
			Res2 = Res1 ++ [{max_redir,[]}];
		{Answer2} ->
			Res2 = Res1 ++ [{max_redir,Answer2}];
		_ ->
			Res2 = Res1 ++ [{max_redir,[]}]
	end,
	case rpc:call(Node,download_manager,get_down_timeout,[],1000) of
		{badrpc,_} ->
			Res3 = Res2 ++ [{down_timeout,[]}];
		{Answer3} ->
			Res3 = Res2 ++ [{down_timeout,Answer3}];
		_ ->
			Res3 = Res2 ++ [{down_timeout,[]}]
	end,
	case rpc:call(Node,download_manager,get_conn_timeout,[],1000) of
		{badrpc,_} ->
			Res4 = Res3 ++ [{conn_timeout,[]}];
		{Answer4} ->
			Res4 = Res3 ++ [{conn_timeout,Answer4}];
		_ ->
			Res4 = Res3 ++ [{conn_timeout,[]}]
	end,
	Res4.

node_stats(Node) ->
	case rpc:call(Node,erlang,memory,[total],1000) of
		{badrpc,_} ->
			Res = [{memory_total,[]}];
		Answer ->
			Res = [{memory_total,[{Answer div (1024*1024),"MB"},{(Answer rem (1024*1024)) div 1024,"kB"}]}];
		_ ->
			Res =[{memory_total,[]}]
	end,
	case rpc:call(Node,erlang,statistics,[wall_clock],1000) of
		{badrpc,_} ->
			Res1 = Res ++ [{uptime,[]}];
		{Total,_Since_Last_Call} ->
			Res1 = Res ++ [{uptime,[{Total div (1000*3600),"h"},{(Total rem (1000*3600)) div (60*1000),"min"},{((Total rem (1000*3600*60)) div 1000) rem 60,"s"}]}];
		_ ->
			Res1 = Res ++ [{uptime,[]}]
	end,
	case rpc:call(Node,stats_server,get_stats,[],1000) of
		{badrpc,_} ->
			Res2 = Res1 ++ [{input_data,[]}];
		{_,BinaryTotal,_,_} ->
			Res2 = Res1 ++ [{input_data,[{BinaryTotal div (1024*1024),"MB"},{(BinaryTotal rem (1024*1024)) div 1024,"kB"}]}];
		_ ->
			Res2 = Res1 ++ [{input_data,[]}]
	end,
	case rpc:call(Node,data_api,get_count,[indexes],1000) of
		{badrpc,_} ->
			Res3 = Res2 ++ [{indexes,[]}];
		{ok,Value} ->
			Res3 = Res2 ++ [{indexes, Value}];
		_ ->
			Res3 = Res2 ++ [{indexes,[]}]
	end,
	case rpc:call(Node,data_api,get_count,[words_coll],1000) of
		{badrpc,_} ->
			Res4 = Res3 ++ [{words_coll,[]}];
		{ok,Value2} ->
			Res4 = Res3 ++ [{words_coll,Value2}];
		_ ->
			Res4 = Res3 ++ [{words_coll,[]}]
	end,
	case rpc:call(Node,data_api,get_count,[index_coll],1000) of
		{badrpc,_} ->
			Res5 = Res4 ++ [{index_coll,[]}];
		{ok,Value3} ->
			Res5 = Res4 ++ [{index_coll,Value3}];
		_ ->
			Res5 = Res4 ++ [{index_coll,[]}]
	end,
	case rpc:call(Node,stats_server,get_stats,[],1000) of
		{badrpc,_} ->
			Res6 = Res5 ++ [{processed_links,[]}];
		{Page_total,_,_,_} ->
			Res6 = Res5 ++ [{processed_links,Page_total}]
	end,
	case rpc:call(Node,stats_server,get_stats,[],1000) of
		{badrpc,_} ->
			Res7 = Res6 ++ [{words_total,[]}];
		{_,_,Words_total,_} ->
			Res7 = Res6 ++ [{words_total,Words_total}]
	end,
	Res7.
query_nodes(Nodes,data_api,get_index,Args) ->
	query_nodes_4_word(Nodes, Args, []);

query_nodes([],_,_,_)->
	[];
query_nodes([H|T],Module,Function,Args) ->
	case rpc:call(H,Module,Function,Args) of
		{badrpc,_} ->
			[{H,none}] ++ query_nodes(T, Module, Function, Args);
		Answer ->
			[{H,Answer}] ++ query_nodes(T, Module, Function, Args)
	end.


query_nodes_4_word([],_,Accum) ->
	Accum;
query_nodes_4_word([H|T],Word,Accum) ->
	try 
		case rpc:call(H,data_api,get_index,[Word]) of
			{badrpc,_} ->
				query_nodes_4_word(T, Word, Accum);
			Answer ->
				query_nodes_4_word(T, Word, Accum++Answer)
		end
	catch
		_:_ ->
			query_nodes_4_word(T, Word, Accum)
	end.
		  
