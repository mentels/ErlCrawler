-module(dispatch_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, dispatch_add_index/2, report_queues/0, prepare_to_stop/0]).
-export([dispatch_add_index/4]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(DispatcherCfg) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, DispatcherCfg, []).


dispatch_add_index(Word, UrlId) ->
	gen_server:cast(?SERVER, {dispatch_add_index, Word, UrlId}).


report_queues() ->
	gen_server:cast(?SERVER, report_queues).


prepare_to_stop() ->
	gen_server:call(?SERVER, prepare_to_stop).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(DispatcherCfg) ->
	process_flag(trap_exit, true),
	{{channels_cnt, ChannelsCnt}, {persistence_server_names, PersistenceServerNameList}} = DispatcherCfg,
	DispatchTabId = ets:new(dispatch_table, [set, {keypos, 1}, protected, named_table]),
	set_dispatch_table(DispatchTabId, ChannelsCnt, PersistenceServerNameList),
	State = {{dispatch_tab_id, DispatchTabId}, {channels_cnt, ChannelsCnt}},
    {ok, State}.


handle_call(prepare_to_stop, _From,  State) ->
	DispatchTabId = get_state_value(dispatch_tab_id, State),
	lists:foreach(fun(I) ->
					{_ChannelId, PersistenceServerName} = I,
					persistence_server:prepare_to_stop(PersistenceServerName) 
				end, 
		ets:match_object(DispatchTabId, '$1')),
	{reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({dispatch_add_index, Word, UrlId}, State)->
	DispatchTabId = get_state_value(dispatch_tab_id, State),
	ChannelsCnt = get_state_value(channels_cnt, State),
	spawn(?MODULE, dispatch_add_index, [Word, UrlId, DispatchTabId, ChannelsCnt]),
	{noreply, State};

handle_cast(report_queues, State) ->
	DispatchTabId = get_state_value(dispatch_tab_id, State),
	lists:foreach(fun(I) ->
					{ChannelId, PersistenceServerName} = I,
					Pid = whereis(PersistenceServerName),
					lager:info("Channel ~p queue size: ~p", 
							   [ChannelId, element(2,erlang:process_info(Pid, message_queue_len))])
				end, 
		ets:match_object(DispatchTabId, '$1')),
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(shutdown, _State) ->
	ok;

terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

set_dispatch_table(_, 0, []) ->
	ok;

set_dispatch_table(DispatchTabId, ChannelsCnt, [PersistenceServerName| T]) ->
	ets:insert(DispatchTabId, {ChannelsCnt, PersistenceServerName}),
	set_dispatch_table(DispatchTabId, ChannelsCnt - 1, T).


dispatch_add_index(Word, UrlId, DispatchTabId, ChannelsCnt) ->
	Key = erlang:phash(Word, ChannelsCnt),
	[[PersistenceServerName]] = ets:match(DispatchTabId, {Key, '$0'}),
	persistence_server:add_index(PersistenceServerName, Word, UrlId).

%%
%% State handling helper functions.
%%
get_state_value(dispatch_tab_id, {{dispatch_tab_id, DispatchTabId}, _}) ->
	DispatchTabId;

get_state_value(channels_cnt, {_, {channels_cnt, ChannelsCnt}}) ->
	ChannelsCnt.
