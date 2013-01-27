%% @author maciek
%% @doc @todo Add description to utils.


-module(utils).

%% ====================================================================
%% API functions
%% ====================================================================
-export([query_nodes/4,query_nodes_4_word/3,node_stats/1]).
 

node_stats(Node) ->
	case rpc:call(Node,erlang,memory,[total]) of
		{badrpc,_} ->
			Res = [{memory_total,[]}];
		Answer ->
			Res = [{memory_total,[{Answer div (1024*1024),"MB"},{(Answer rem (1024*1024)) div 1024,"kB"}]}];
		_ ->
			Res =[{memory_total,[]}]
	end,
	case rpc:call(Node,erlang,statistics,[wall_clock]) of
		{badrpc,_} ->
			Res1 = Res ++ [{uptime,[]}];
		{Total,_Since_Last_Call} ->
			Res1 = Res ++ [{uptime,[{Total div (1000*3600),"h"},{(Total rem (1000*3600)) div (60*1000),"min"},{((Total rem (1000*3600*60)) div 1000) rem 60,"s"}]}];
		_ ->
			Res1 = Res ++ [{uptime,[]}]
	end,
	case rpc:call(Node,erlang,statistics,[io]) of
		{badrpc,_} ->
			Res2 = Res1 ++ [{input_data,[]}];
		{{input,Input},{output,_Output}} ->
			Res2 = Res1 ++ [{input_data,[{Input div (1024*1024),"MB"},{(Input rem (1024*1024)) div 1024,"kB"}]}];
		_ ->
			Res2 = Res1 ++ [{input_data,[]}]
	end,
	case rpc:call(Node,data_api,get_count,[indexes]) of
		{badrpc,_} ->
			Res3 = Res2 ++ [{indexes,[]}];
		{ok,Value} ->
			Res3 = Res2 ++ [{indexes, Value}];
		_ ->
			Res3 = Res2 ++ [{indexes,[]}]
	end,
	case rpc:call(Node,data_api,get_count,[words_coll]) of
		{badrpc,_} ->
			Res4 = Res3 ++ [{words_coll,[]}];
		{ok,Value2} ->
			Res4 = Res3 ++ [{words_coll,Value2}];
		_ ->
			Res4 = Res3 ++ [{words_coll,[]}]
	end,
	case rpc:call(Node,data_api,get_count,[index_coll]) of
		{badrpc,_} ->
			Res5 = Res4 ++ [{index_coll,[]}];
		{ok,Value3} ->
			Res5 = Res4 ++ [{index_coll,Value3}];
		_ ->
			Res5 = Res4 ++ [{index_coll,[]}]
	end,
	case rpc:call(Node,link_server,processed_links,[]) of
		{badrpc,_} ->
			Res6 = Res5 ++ [{processed_links,[]}];
		Value4 ->
			Res6 = Res5 ++ [{processed_links,Value4}]
	end,
	Res6.
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
		  
