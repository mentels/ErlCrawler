-module(stats_server).
-behaviour(gen_server).

-export([start_link/1,init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([add_page_stats/2,write_stats/0]).

start_link(_) ->
	  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


add_page_stats(Binary_len,Words_len) ->
	gen_server:call(?MODULE, {add_page_stats,Binary_len,Words_len}).

write_stats()->
	gen_server:call(?MODULE, write_stats).

init(_) ->
	{ok,Frequency} = application:get_env(crawler, stat_interval),
	timer:apply_interval(Frequency, ?MODULE, write_stats, []),
	{ok, File} = file:open("log/stats_total.log" ,[append]),
	{ok,{0,0,0,File}}.

handle_call(write_stats, From, {PageTotal,BinaryTotal,WordsTotal,File}) ->
 	{ok,Indexes}= data_api:get_count(indexes),
	{ok,Words_coll} = data_api:get_count(words_coll),
	{ok,Index_coll} = data_api:get_count(index_coll),
	Memory = erlang:memory(total),
	{Mega_sec,Sec,Milisec} = erlang:now(),
	{{Year,Month,Day},{Hour,Minute,Seconds}}=calendar:now_to_local_time(erlang:now()),
	io:format(File,"~w-~w-~w ~w:~w:~w ~w~w~w Page_total: ~w Binary_total ~w Words_processed_total ~w Indexes ~w Words_coll ~w Index_coll ~w Memory ~w\n",[Year,Month,Day,Hour,Minute,Seconds,Mega_sec,Sec,Milisec,PageTotal,BinaryTotal,WordsTotal, Indexes, Words_coll,Index_coll,Memory]),
	{reply,ok,{PageTotal,BinaryTotal,WordsTotal,File}};

handle_call({add_page_stats,Binary_len,Words_len},From,{PageTotal,BinaryTotal,WordsTotal,File}) ->
	{reply,ok,{PageTotal+1,BinaryTotal+Binary_len,WordsTotal+Words_len,File}}.

handle_cast(Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.