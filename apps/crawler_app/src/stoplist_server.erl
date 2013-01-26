%% @author maciek
%% @doc @todo Add description to stoplist_server.


-module(stoplist_server).
-behaviour(gen_server).
-export([start_link/1,init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([check_word/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([insert_stoplists/2, insert_words/2, readlines/1, get_all_lines/2]).

start_link(_) ->
	  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
	{ok,Stoplists} = application:get_env(stoplists),
	T = ets:new(tab,[]),
	insert_stoplists(T,Stoplists),
	
	{ok,EngFile} = application:get_env(eng_words),
	{ok, Binary} = file:read_file(EngFile),
	B = binary:split(Binary,[<<"\n">>],[global]),
	L = lists:map(fun(X)->binary_to_list(X) end,B),
	L2 = lists:map(fun(X) -> {X} end,L),
	EngWords = ets:new(eng_words,[set, {keypos, 1}, private]),
	ets:insert(EngWords, L2),
	

%% 	{ok,PolFile} = application:get_env(pol_words),
%% 	{ok, PolBinary} = file:read_file(PolFile),
%% 	Pol_B = binary:split(PolBinary,[<<"\n">>],[global]),
%% 	Pol_L = lists:map(fun(X)->binary_to_list(X) end,Pol_B),
%% 	Pol_L2 = lists:map(fun(X) -> {X} end,Pol_L),
	PolWords = ets:new(pol_words,[set, {keypos, 1}, private]),
%% 	ets:insert(PolWords, Pol_L2),
	
	
    {ok, {T,EngWords,PolWords}}.

%% Stoplistdasa ma miec format listy nazw plikow, w ktorych sa slowa
%% w kazdym pliku jedno słowo w jednej linii.

check_word(Word) ->
	gen_server:call(?MODULE, {check_word,Word}).

insert_stoplists(Table,[]) ->
	ok;
insert_stoplists(Table,[{Filename}|T]) ->
	Words = readlines(Filename),
	insert_words(Table,Words),
	insert_stoplists(Table,T).

insert_words(Table,[]) ->
	ok;
insert_words(Table,[H|T]) ->
	ets:insert(Table, {H}),
	insert_words(Table,T).
	
readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    get_all_lines(Device, []).

get_all_lines(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> file:close(Device), Accum;
        Line -> get_all_lines(Device, Accum ++ [string:strip(Line,right,$\n)])
    end.	

handle_call({check_word,Word}, From, {T,EngWords,PolWords}) ->
	%% Slowo ma nie byc na stopliscie, a ma byc w etsach ze slowami jezyka
%% 	Status = (not ets:member(T, Word)) andalso (ets:member(EngWords,Word) orelse ets:member(PolWords,Word)),
	Status = (not ets:member(T, Word)), 
    {reply,Status , {T,EngWords,PolWords}}.


handle_cast(Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.