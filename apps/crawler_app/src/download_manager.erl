%% @author maciek
%% @doc @todo Add description to download_manager.

-module(download_manager).
-behaviour(gen_server).
-export([start_link/1,init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% API functions
%% ====================================================================
-export([spawn_downloaders/1,set_max_active_workers/1, get_max_active_workers/0, get_current_active_workers/0, start_processing/0,get_conn_timeout/0,get_down_timeout/0,get_max_redir/0,set_conn_timeout/1,set_down_timeout/1,set_max_redir/1]).

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

%% S/GETTERY
get_current_active_workers()->
	gen_server:call(?MODULE,get_current_active_workers).

set_max_active_workers(Number)->
	gen_server:call(?MODULE,{set_max_active_workers,Number}).

get_max_active_workers()->
	gen_server:call(?MODULE,get_max_active_workers).

set_max_redir(Number) ->
	gen_server:call(?MODULE, {set_max_redir,Number}).

get_max_redir() ->
	gen_server:call(?MODULE, get_max_redir).

set_down_timeout(Timeout)->
	gen_server:call(?MODULE,{set_down_timeout,Timeout}).

get_down_timeout()->
	gen_server:call(?MODULE,get_down_timeout).

set_conn_timeout(Timeout)->
	gen_server:call(?MODULE,{set_conn_timeout,Timeout}).

get_conn_timeout()->
	gen_server:call(?MODULE,get_conn_timeout).

	
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


handle_call(get_max_active_workers,_From,{state,_Active_workers,Max_workers,_Cfg}=State) ->
	{reply,{Max_workers},State};

handle_call({set_max_active_workers,Number},_From,{state,Active_workers,_Max_workers,Cfg})->
	{reply,ok,{state,Active_workers,Number,Cfg}};

handle_call(get_current_active_workers,_From,{state,Active_workers,_Max_workers,_Cfg}=State) ->
	{reply,{Active_workers},State};

handle_call(get_max_redir,_From,{state,_Active_workers,_Max_workers,{_,_,_,_,Redirect_limit}}=State) ->
	{reply,{Redirect_limit},State};

handle_call({set_max_redir,Number},_From,{state,_Active_workers,_Max_workers,{Max_workers,Uds_fail_interval,Connection_timeout,Download_timeout,_Redirect_limit}}=State) ->
	{reply,ok,{state,_Active_workers,_Max_workers,{Max_workers,Uds_fail_interval,Connection_timeout,Download_timeout,Number}}};

handle_call(get_conn_timeout,_From,{state,_Active_workers,_Max_workers,{_,_,Connection_timeout,_,_}}=State)->
	{reply,{Connection_timeout},State};

handle_call({set_conn_timeout,Number},_From,{state,Active_workers,Max_workers,{Max_workers2,Uds_fail_interval,Connection_timeout,Download_timeout,Redirect_limit}}=State)->
	{reply,ok,{state,Active_workers,Max_workers,{Max_workers2,Uds_fail_interval,Number,Download_timeout,Redirect_limit}}};

handle_call(get_down_timeout,_From,{state,_,_,{_,_,_,Download_timeout,_}} = State) ->
	{reply,{Download_timeout},State};

handle_call({set_down_timeout,Number},_,{state,Active_workers,Max_workers,{Max_workers2,Uds_fail_interval,Connection_timeout,Download_timeout,Redirect_limit}}) ->
	{reply,ok,{state,Active_workers,Max_workers,{Max_workers2,Uds_fail_interval,Connection_timeout,Number,Redirect_limit}}}.


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
spawn_downloaders({state,Active_workers,Max_workers,Cfg}=State) when Active_workers >= Max_workers ->
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