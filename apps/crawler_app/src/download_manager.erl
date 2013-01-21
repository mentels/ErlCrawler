%% @author maciek
%% @doc @todo Add description to download_manager.

-module(download_manager).
-behaviour(gen_server).
-export([start_link/1,init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% API functions
%% ====================================================================
-export([spawn_downloaders/1,set_max_active_workers/1, get_max_active_workers/1, get_current_active_workers/1, start_processing/0]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {active_workers,max_workers,cfg}).

start_link(Cfg) ->
    gen_server:start_link({local, download_manager}, download_manager, [Cfg], []).

init([{MaxWorkers,_,_,_,_}=Cfg]) ->
	process_flag(trap_exit, true),
	timer:apply_interval(30000, download_manager, start_processing, []), %% TODO : Wczytywanie z configu ?
    {ok, #state{active_workers=0, max_workers=MaxWorkers, cfg=Cfg}}.

start_processing()->
	gen_server:call(?MODULE, start_processing).

set_max_active_workers(Number)->
	gen_server:call(?MODULE,set_max_active_workers,[Number]).

get_max_active_workers(Pid)->
	gen_server:call(?MODULE,get_max_active_workers,[Pid]).

get_current_active_workers(Pid)->
	gen_server:call(?MODULE,get_current_active_workers,[Pid]).

%% ==============================
%% HANDLE_CALLS =================
%% ==============================

%% ==============================
%% 1. START PROCESSING===========
%% ==============================

handle_call(start_processing,_From,{state,Active_workers,Max_workers,Cfg}=State) when Active_workers < Max_workers ->
	case url_download_server:pull(1) of
		[{Url_id, Url}] -> 
			spawn_monitor(downloader, download, [Url,Url_id,0,Cfg]),
			{reply,ok,{state,Active_workers+1,Max_workers,Cfg}};
		_ ->
			{reply,url_not_found,{state,Active_workers,Max_workers,Cfg}}
	end;
handle_call(start_processing,_From,{state,Active_workers,Max_workers,Cfg}=State) ->
	{reply,ok,State};

%% ==============================
%% 2. START PROCESSING===========
%% ==============================

handle_call({set_max_active_workers,Number},_From,{state,Active_workers,Max_workers}=State) when Number>=0 ->
	NewState = {state,Active_workers,Number},
	{reply,ok,NewState};

handle_call({get_max_active_workers,Pid},_From,{state,_Active_workers,Max_workers}=State) ->
	{reply,Max_workers,State};

handle_call({get_current_active_workers,Pid},_From,{state,Active_workers,_Max_workers}=State) ->
	{reply,Active_workers,State}.

%% ==============================
%% SPAWN DOWNLOADERS ============
%% ==============================

spawn_downloaders({state,Active_workers,Max_workers,Cfg}=State) when Active_workers == Max_workers ->
	State;
spawn_downloaders({state,Active_workers,Max_workers,{_,UDS_fail_interval,_,_,_}=Cfg}=State) when Active_workers =< Max_workers ->
	case url_download_server:pull(1) of
		[{Url_id, Url}] -> 
			spawn_monitor(downloader, download, [Url,Url_id,0,Cfg]),
			spawn_downloaders({state,Active_workers+1,Max_workers,Cfg});
		_ -> 
			State
	end;
spawn_downloaders({state,Active_workers,Max_workers}=State) when Active_workers >= Max_workers ->
	State.


%% =======================
%% MOCK FUNCTIONS ========
%% =======================
handle_cast(Msg, State) ->
    {noreply, State}.

%% handle_info/2
handle_info({'DOWN', Ref, process, Pid2, Reason}, {state,Active_workers,Max_workers,Cfg}) ->
	New_state=spawn_downloaders({state,Active_workers-1,Max_workers,Cfg}),
	{noreply, New_state}.

%% terminate/2
terminate(Reason, State) ->
    ok.

%% code_change/3
code_change(OldVsn, State, Extra) ->
    {ok, State}.