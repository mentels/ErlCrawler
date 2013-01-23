-module(notification_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, notify/2, get_info/2]).
-export([get_state/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(ServerName) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [], []).


notify(ServerName, Notification) when Notification ==  save_word_about_to_be_spawned ->
	gen_server:cast(ServerName, Notification);

notify(ServerName, Notification) when Notification ==  save_word_completed ->
	gen_server:cast(ServerName, Notification);

notify(ServerName, Notification) when Notification ==  new_bucket_operation_about_to_be_spawned ->
	gen_server:cast(ServerName, Notification);

notify(ServerName, Notification) when Notification ==  new_bucket_operation_completed ->
	gen_server:cast(ServerName, Notification).



get_info(ServerName, Info) when Info == all_save_word_completed ->
	gen_server:call(ServerName, Info);

get_info(ServerName, Info) when Info == all_new_bucket_operations_to_completed ->
	gen_server:call(ServerName, Info).


get_state(ServerName) ->
	gen_server:call(ServerName, get_state).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
	process_flag(trap_exit, true),
	State = {{save_word_spawned, 0}, {new_bucket_operations_spawned, 0}},
    {ok, State}.


handle_call(all_save_word_completed, _From, State) ->
	case State of
		{save_word_spawned, 0} ->
			{reply, true, State};			

		{save_word_spawned, _} ->
			{reply, false, State}
	end;

handle_call(_Request, _From, State) ->
	{reply, ok, State}.


handle_cast(save_word_about_to_be_spawned, State) ->
	{{_, SpawnedCnt}, NewBucketOperations} = State,
	lager:debug("Notification: save word about to be spawned."),
	{noreply, {{save_word_spawned, SpawnedCnt + 1}, NewBucketOperations}};

handle_cast(save_word_completed, State) ->
   	{{_, SpawnedCnt}, NewBucketOperations} = State,
	lager:debug("Notification: save word completed."),
	{noreply, {{save_word_spawned, SpawnedCnt - 1}, NewBucketOperations}};

handle_cast(new_bucket_operation_about_to_be_spawned, State) ->
   	{SaveWordOperations, {new_bucket_operations_spawned, SpawnedCnt}} = State,
	lager:debug("Notification: new bucket operation about to be spawned."),
	{noreply, {SaveWordOperations, {new_bucket_operations_spawned, SpawnedCnt + 1}}};

handle_cast(new_bucket_operation_completed, State) ->
   	{SaveWordOperations, {new_bucket_operations_spawned, SpawnedCnt}} = State,
	lager:debug("Notification: new bucket operation completed."),
	{noreply, {SaveWordOperations, {new_bucket_operations_spawned, SpawnedCnt - 1}}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


terminate(shutdown, _State) ->
	lager:debug("Notification server terminating for shutdown reason."),
	ok;

terminate(Reason, _State) ->
	lager:debug("Notification server terminating for reason: ~p", [Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
