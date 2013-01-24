-module(persistence_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_index/3, retry_add_index/3, prepare_to_stop/1]).
-export([enqueue_indicies_to_delete/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link([ServerName, HelperServersCfg, PersistenceCfg]) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [HelperServersCfg, PersistenceCfg], []).


add_index(ServerName, Word, UrlId) ->
	gen_server:cast(ServerName, {add_index, Word, UrlId}).


retry_add_index(ServerName, Word, UrlId) ->
	gen_server:cast(ServerName, {retry_add_index, Word, UrlId}).


prepare_to_stop(ServerName) ->
	gen_server:call(ServerName, prepare_to_stop).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([HelperServersCfg, PersistenceCfg]) ->
	process_flag(trap_exit, true),
	[MaxCacheDocSizeCfg, RetryCfg] = PersistenceCfg, 
	
	{WordsCacheCfg, IndexCacheCfg, CleanerCacheCfg, NotificationCfg, 
	 	ConnManagerServerCfg} = HelperServersCfg,
	
	State = {MaxCacheDocSizeCfg, RetryCfg, WordsCacheCfg, IndexCacheCfg, CleanerCacheCfg, 
			 NotificationCfg, ConnManagerServerCfg},
	
    {ok, State}.


handle_call(prepare_to_stop, _From, State) ->
	lager:debug("Serving request: prepare_to_stop."),
	save_index_cache_to_db(State),
	db_cleaner_server:flush_cache(get_server_name(index, State)),
	{reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({add_index, Word, UrlId}, State) ->
	lager:debug("Serving request: {add_index, ~p, ~p}. Mailbox queue size: ~p", 
				[Word, UrlId, element(2,erlang:process_info(self(), message_queue_len)) ]),
	add_index(first_try, Word, UrlId, State),
	{noreply, State};

handle_cast({retry_add_index, Word, UrlId}, State) ->
	lager:debug("Serving request: {retry_add_index, ~p, ~p}. Mailbox queue size: ~p", 
				[Word, UrlId, element(2,erlang:process_info(self(), message_queue_len)) ]),
	add_index(retry, Word, UrlId, State),
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

get_word_data(Word, State) ->
	lager:debug("Obtaining data for word: ~p.", [Word]),
	case get_word_data_from_cache(Word, State) of
		word_not_found ->
			case get_word_data_from_db(Word, State) of
				no_word ->
					{Word, WordId, BucketId} = create_new_cache_word_doc(Word),
					lager:debug("Word data not found in cache nor in db. New cache word doc created: {~p, ~p, ~p}.", 
								[Word, WordId, BucketId]),
					update_words_cache({add_word_cache_doc, {Word, WordId, BucketId}}, State),
					spawn_save_word_to_db({Word, WordId, BucketId}, State),
					{WordId, BucketId};
				
				{WordId, BucketId} ->
					lager:debug("Word data not found in cache but in db: {~p, ~p}.", [WordId, BucketId]),
					update_words_cache({add_word_cache_doc, {Word, WordId, BucketId}}, State),
					{WordId, BucketId}
				
			end;
		
		WordData ->
			lager:debug("Word data found in cache: ~p.", [WordData]),
			WordData
	
	end.
				

add_index(first_try, Word, UrlId, State) ->
	{WordId, BucketId} = get_word_data(Word, State),
	try add_index(WordId, BucketId, UrlId, State)
	catch
		error:no_word_in_index ->
			spawn_delayed_add_index(Word, UrlId, State);
		
		error:no_bucket_in_index ->	
			spawn_delayed_add_index(Word, UrlId, State)
	end;

add_index(retry, Word, UrlId, State) ->
	{WordId, BucketId} = get_word_data(Word, State),
	try add_index(WordId, BucketId, UrlId, State)
	catch
		error:_ ->
			lager:error("request: {retry_add_index, ~p, ~p} failed.", [Word, UrlId])
	end;

add_index(WordId, BucketId, UrlId, State) ->
	lager:debug("Obtaining index data for word data: {~p, ~p}.", [WordId, BucketId]),
	case get_index_data_from_cache(WordId, State) of
		index_not_found ->
			case is_bucket_in_db(BucketId) of
				true ->
					IncompleteCacheDoc = get_index_from_db(BucketId, WordId, State),
					UpdatedCacheDoc = update_incomplete_cache_doc(IncompleteCacheDoc, BucketId, UrlId),
					lager:debug("Index data for word data: {~p, ~p} not found in cache but in db.", [WordId, BucketId]),
					update_index_cache({add_index, UpdatedCacheDoc}, State);

				false ->
					lager:debug("Index data for word data: {~p, ~p} not found in cache nor in db. Creating new cache index doc."
							   , [WordId, BucketId]),
					update_index_cache({add_index, create_new_index_cache_doc(WordId, UrlId)}, State)
			end;
		
		IndexData ->
			lager:debug("Index data found in cache."),
			case update_index_data(IndexData, UrlId) of
				not_updated ->
					lager:debug("Url id ~p is already contained in the index data: ~w.", [UrlId, IndexData]),
					ok;
				
				UpdatedIndexData ->
					update_index_cache({update_index_data, WordId, UpdatedIndexData}, State)
			end
			
	end.


enqueue_indicies_to_delete([], _) ->
	ok;
	
enqueue_indicies_to_delete([{WordId, _, _, OldBucketId, InitUrlIdListSize} | T], CleanerServerName) ->
	case (OldBucketId == unspec) and (InitUrlIdListSize == unspec) of
		true ->
			enqueue_indicies_to_delete(T, CleanerServerName);
	
		false ->
			db_cleaner_server:add_index_to_delete(CleanerServerName, OldBucketId, InitUrlIdListSize, WordId),
			enqueue_indicies_to_delete(T, CleanerServerName)
	end.

%%%
%%% Word data obtaining helper functions.
%%%
get_word_data_from_cache(Word, State) ->
	{ok, WordData} = words_cache_server:get_word_data(get_server_name(words, State), Word),
	WordData.

get_word_data_from_db(Word, State) ->
	ConnManagerServerName = get_server_name(conn_manager, State),
	wordsdb_functions:get_word_data(Word, ConnManagerServerName).

%%
%% Index obtaining helper functions.
%%
get_index_data_from_cache(WordId, State) ->
	{ok, IndexData} = index_cache_server:get_index_data(get_server_name(index, State), WordId),
	IndexData.


get_index_from_db(BucketId, WordId, State) ->
	ConnManagerServerName = get_server_name(conn_manager, State),
	case indexdb_functions:get_index(BucketId, WordId, ConnManagerServerName) of
		{ok, no_word} ->
			lager:error("No word id: ~p found in bucket id: ~p", [WordId, BucketId]),
			erlang:error(no_word_in_index);
		
		{ok, no_bucket} ->
			lager:error("No bucket id found: ~p", [BucketId]),
			erlang:error(no_bucket_in_index);
		
		{ok, IncompleteCacheDoc} -> 
			IncompleteCacheDoc
	end.


%%
%% Cache doc updating helper functions.
%%
update_index_data({UrlIdList, UrlIdListSize}, UrlId) ->
	case update_url_id_list(UrlIdList, UrlId) of
		{not_updated, _} ->
			not_updated;
			
		{updated, UpdatedUrlIdList} -> 
			{UpdatedUrlIdList, UrlIdListSize + 1}
	end.


update_incomplete_cache_doc({WordId, UrlIdList, UrlIdListSize}, OldBucketId, UrlId) ->
	InitUrlIdListSize = UrlIdListSize,
	case update_url_id_list(UrlIdList, UrlId) of
		{not_updated, _} ->
			{WordId, UrlIdList, UrlIdListSize, OldBucketId, InitUrlIdListSize};
			
		{updated, UpdatedUrlIdList} -> 
			{WordId, UpdatedUrlIdList, UrlIdListSize + 1, OldBucketId, InitUrlIdListSize}
	end.


update_url_id_list([], UrlId) ->
	{updated, [UrlId]};

update_url_id_list([ H | T ], UrlId) ->
	%% Decending order.
	if 
		UrlId > H ->
			{updated, [ UrlId | [ H | T ] ]};
		UrlId < H ->
			{Status, List} = update_url_id_list(T, UrlId),
			{Status, [ H | List ]};
		UrlId == H ->
			{not_updated, [ H | T ]}
	end.


%%
%% Cache handling helper functions.
%%
create_new_index_cache_doc(WordId, UrlId) ->
	UrlIdList = [UrlId],
	UrlIdListSize = 1,
	OldBucketId = unspec,
	InitUrlIdListSize = unspec,
	{WordId, UrlIdList, UrlIdListSize, OldBucketId, InitUrlIdListSize}.


update_index_cache({update_index_data, WordId, IndexData}, State) ->
	case index_cache_server:update_index_data(get_server_name(index, State), WordId, IndexData) of
		ok ->
			lager:debug("Index cache updated for word id: ~p with new index data: ~w.", [WordId, IndexData]),
			ok;
		
		{ok, full} ->
			lager:debug("Index cache updated for word id: ~p with new index data: ~w.", [WordId, IndexData]),
			save_index_cache_to_db(State)
	end;

update_index_cache({add_index, CacheDoc}, State) ->
	case index_cache_server:add_index(get_server_name(index, State), CacheDoc) of
		ok ->
			lager:debug("Index cache updated with new cache index doc: ~w.", [CacheDoc]),
			ok;
		
		{ok, full} ->
			lager:debug("Index cache updated with new cache index doc: ~w.", [CacheDoc]),
			save_index_cache_to_db(State)
	end.


%%
%% Words cache handling helper functions.
%%
create_new_cache_word_doc(Word) ->
	{ok, WordId} = id_server:get_word_id(),
	{Word, WordId, unspec}.


update_words_cache({add_word_cache_doc, CacheWordDoc}, State) ->
	CacheServerName = get_server_name(words, State),
	case words_cache_server:add_word_cache_doc(CacheServerName, CacheWordDoc) of
		{ok, full} ->
			words_cache_server:flush_and_add_word_cache_doc(CacheServerName, CacheWordDoc),
			lager:debug("Words cache flushed and updated with new cache word doc: ~p.", [CacheWordDoc]);
		
		ok ->
			lager:debug("Words cache updated with new cache word doc: ~p.", [CacheWordDoc]),
			ok
	end.

%%
%% Old indicies handling helper functions.
%% 
save_index_cache_to_db(State) ->
	lager:debug("Cleaning index cache."),
	{ok, CacheDocList} =  index_cache_server:retrieve_all_indicies(get_server_name(index, State)),
	{ok, BucketId} = id_server:get_bucket_id(),
	Counters = {0, 0},
	Lists = {[], [], []},
	spawn_enqueue_indicies_to_delete(CacheDocList, get_server_name(cleaner, State)),
	save_index_cache_to_db(CacheDocList, BucketId, Counters, Lists, State).


save_index_cache_to_db([CacheDoc | T], BucketId, Counters, Lists, State) ->
	{WordId, UrlIdList, UrlIdListSize, _, _} = CacheDoc,
	{WordCnt, UrlCnt} = Counters,
	{IncompleteCacheDocList, WordIdToUpdateList, WordIdToFreezeList} = Lists,
	IncompleteCacheDoc = {WordId, UrlIdList, UrlIdListSize},
	NewCounters = {WordCnt + 1, UrlCnt + UrlIdListSize},
	case is_url_id_list_size_max(UrlIdListSize, State) of
		true ->
			NewLists = {[IncompleteCacheDoc | IncompleteCacheDocList], WordIdToUpdateList, [WordId | WordIdToFreezeList]},
			save_index_cache_to_db(T, BucketId, NewCounters, NewLists, State);
		
		false ->
			NewLists = {[ IncompleteCacheDoc | IncompleteCacheDocList], [WordId | WordIdToUpdateList], WordIdToFreezeList},
			save_index_cache_to_db(T, BucketId, NewCounters, NewLists, State)
	end;

save_index_cache_to_db([], BucketId, Counters, Lists, State) ->
	{WordCnt, UrlCnt} = Counters,
	{IncompleteCacheDocList, WordIdToUpdateList, WordIdToFreezeList} = Lists,
	
	%% Save indicies in bucket and wait for save word operations to complete before updating
	%% bucket ids of words.
	spawn_save_bucket_to_db(BucketId, WordCnt, UrlCnt, IncompleteCacheDocList, State),
	wait_for_save_word_operations_to_complete(get_state_value(notification_server_name, State)),
	
	%% Update active bucket id of words just saved in new bucket.
	spawn_update_active_bucket_id(BucketId, WordIdToUpdateList, State),
	
	%% Freeze bucket ids for words that had extened size and mark them as "unbucketed".
	spawn_update_active_bucket_id(unspec, WordIdToFreezeList, State),
	spawn_freeze_indicies_in_bucket(BucketId, WordIdToFreezeList, State),
	
	%% Flush words cache and wait for new bucket operations to complete.
	words_cache_server:flush(get_server_name(words, State)),
	wait_for_new_bucket_operations_to_complete(get_state_value(notification_server_name, State)).
		

%%
%% Spawning helper functions.
%%
spawn_delayed_add_index(Word, UrlId, State) ->
	RetryDelay = get_state_value(retry_delay, State),
	PersistenceServerPid = self(),
	spawn(
	  	fun() -> 
			timer:sleep(RetryDelay),
			persistence_server:retry_add_index(PersistenceServerPid, Word, UrlId) 
		end
	  ),
	lager:debug("Operation: delayed add index - spawned.").


spawn_save_word_to_db(CacheWordDoc, State) ->
	NotificationServerName = get_state_value(notification_server_name, State),
	ConnManagerServerName = get_server_name(conn_manager, State),
	notification_server:notify(NotificationServerName, save_word_about_to_be_spawned),
	spawn(
	  	fun() ->
			wordsdb_functions:save_word(CacheWordDoc, ConnManagerServerName),
			notification_server:notify(NotificationServerName, save_word_completed)
		end
	  ),
	lager:debug("Operation: save cache word doc to db - spawned.").


spawn_save_bucket_to_db(BucketId, WordCnt, UrlCnt, IncompleteCacheDocList, State) ->
	NotificationServerName = get_state_value(notification_server_name, State),
	ConnManagerServerName = get_server_name(conn_manager, State),
	notification_server:notify(NotificationServerName, new_bucket_operation_about_to_be_spawned),
	spawn(
	  	fun() ->
			indexdb_functions:save_indicies(BucketId, WordCnt, UrlCnt, IncompleteCacheDocList, ConnManagerServerName),
			notification_server:notify(NotificationServerName, new_bucket_operation_completed)
		end
	  ),
	lager:debug("Operation: save bucket to db - spawned.").


spawn_update_active_bucket_id(_BucketId, [], _State) ->
	ok;

spawn_update_active_bucket_id(BucketId, WordIdToUpdateList, State) ->
	NotificationServerName = get_state_value(notification_server_name, State),
	ConnManagerServerName = get_server_name(conn_manager, State),
	notification_server:notify(NotificationServerName, new_bucket_operation_about_to_be_spawned),
	spawn(
	  	fun() ->
			wordsdb_functions:update_active_bucket_id(WordIdToUpdateList, BucketId, ConnManagerServerName),
			notification_server:notify(NotificationServerName, new_bucket_operation_completed)
		end
	  ),
	lager:debug("Operation: update active bucket id - spawned.").


spawn_freeze_indicies_in_bucket(_BucketId, [], _State) ->
	ok;

spawn_freeze_indicies_in_bucket(BucketId, WordIdToFreezeList, State) ->
	ConnManagerServerName = get_server_name(conn_manager, State),
	spawn(
	  	fun() ->
			wordsdb_functions:freeze_bucket_id(WordIdToFreezeList, BucketId, ConnManagerServerName)
		end
	  ),
	lager:debug("Operation: freeze indicies in bucket - spawned.").


spawn_enqueue_indicies_to_delete(CacheDocList, CleanerServerName) ->
	spawn(?MODULE, enqueue_indicies_to_delete, [CacheDocList, CleanerServerName]),
	lager:debug("Operation: enqueue indicies to delete - spawned.").
	

wait_for_save_word_operations_to_complete(NotificationServerName) ->
	case notification_server:get_info(NotificationServerName, all_save_word_completed) of
		true ->
			ok;
		false ->
			timer:sleep(10),
			wait_for_save_word_operations_to_complete(NotificationServerName)
	end.


wait_for_new_bucket_operations_to_complete(NotificationServerName) ->
	case notification_server:get_info(NotificationServerName, all_new_bucket_operations_to_completed) of
		true ->
			ok;
		false ->
			timer:sleep(10),
			wait_for_new_bucket_operations_to_complete(NotificationServerName)
	end.

%%
%% Calculations helper functions.
%%
is_url_id_list_size_max(UrlIdListSize, State) ->
	MaxSize = get_state_value(max_cache_doc_size, State),
	if 
		UrlIdListSize >= MaxSize ->
			true;
		
		true ->
			false
		
	end.


is_bucket_in_db(BucketId) ->
	case BucketId of
		unspec ->
			false;
		
		_ ->
			true
	end.

%%
%% Cache server names helper functions.
%%
get_server_name(words, State) ->
	get_state_value(words_cache_server_name, State);

get_server_name(index, State) ->
	get_state_value(index_cache_server_name, State);

get_server_name(cleaner, State) ->
	get_state_value(cleaner_cache_server_name, State);

get_server_name(conn_manager, State) ->
	get_state_value(conn_manager_server_name, State).

%%
%% State handling helper functions.
%% 
get_state_value(max_cache_doc_size, {{max_cache_doc_size, Value}, _, _, _, _, _, _}) ->
	Value;

get_state_value(retry_delay, {_, {retry_delay, Value}, _, _, _, _, _}) ->
	Value;

get_state_value(words_cache_server_name, {_, _, {words_cache_server_name, Value}, _, _, _, _}) ->
	Value;

get_state_value(index_cache_server_name, {_, _, _, {index_cache_server_name, Value}, _, _, _}) ->
	Value;

get_state_value(cleaner_cache_server_name, {_, _, _, _, {cleaner_cache_server_name, Value}, _, _}) ->
	Value;

get_state_value(notification_server_name, {_, _, _, _, _, {notification_server_name, Value}, _}) ->
	Value;

get_state_value(conn_manager_server_name, {_, _, _, _, _, _, {conn_manager_server_name, Value}}) ->
	Value.
