-module(persistence_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_index/3, retry_add_index/3, prepare_to_stop/1]).
-export([delayed_add_index/4]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link([ServerName, CachesCfg, PersistenceCfg]) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [CachesCfg, PersistenceCfg], []).


add_index(ServerName, Word, UrlId) ->
	gen_server:cast(ServerName, {add_index, Word, UrlId}).


retry_add_index(ServerName, Word, UrlId) ->
	gen_server:cast(ServerName, {retry_add_index, Word, UrlId}).


prepare_to_stop(ServerName) ->
	gen_server:call(ServerName, {prepare_to_stop}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([CachesCfg, PersistenceCfg]) ->
	process_flag(trap_exit, true),
	[MaxCacheDocSizeCfg, RetryCfg] = PersistenceCfg, 
	{IndexCacheCfg, CleanerCacheCfg} = CachesCfg,
	State = {MaxCacheDocSizeCfg, RetryCfg, IndexCacheCfg, CleanerCacheCfg},
    {ok, State}.


handle_call({prepare_to_stop}, _From, State) ->
	lager:debug("Cleaning persistence subsytem before stopping..."),
	handle_old_indices(State),
	db_cleaner_server:flush_cache(get_cache_server_name(index, State)),
	{reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({add_index, Word, UrlId}, State) ->
	lager:debug("Mailbox queue size: ~p", [erlang:process_info(self(), message_queue_len)]),
	add_index(first_try, Word, UrlId, State),
	{noreply, State};

handle_cast({retry_add_index, Word, UrlId}, State) ->
	add_index(retry, Word, UrlId, State),
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(shutdown, _State) ->
	lager:debug("Persistence server terminating for shutdown reason."),
	ok;

terminate(Reason, _State) ->
	lager:debug("Persistence server terminating for reason: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_word_data(Word) ->
	{ok, WordData} = wordsdb_functions:save_word(Word),
	WordData.


add_index(first_try, Word, UrlId, State) ->
	{WordId, BucketId} = get_word_data(Word),
	lager:debug("Adding index: word id: ~p; bucket id: ~p; url id: ~p.", [WordId, BucketId, UrlId]),
	try add_index(WordId, BucketId, UrlId, State)
	catch
		error:no_word_in_index ->
			spawn_delayed_add_index(Word, UrlId, State);
		
		error:no_bucket_in_index ->	
			spawn_delayed_add_index(Word, UrlId, State)
	end;

add_index(retry, Word, UrlId, State) ->
	{WordId, BucketId} = get_word_data(Word),
	lager:debug("Retrying to add index: word id: ~p; bucket id: ~p; url id: ~p.", [WordId, BucketId, UrlId]),
	try add_index(WordId, BucketId, UrlId, State)
	catch
		error:_ ->
			lager:debug("Retry failed.")
	end;

add_index(WordId, BucketId, UrlId, State) ->
	case get_index_data_from_cache(WordId, State) of
		index_not_found ->
			lager:debug("Index for word id: ~p not found in cache.", [WordId]),
			case is_bucket_in_db(BucketId) of
				true ->
					IncompleteCacheDoc = get_index_from_db(BucketId, WordId),
					UpdatedCacheDoc = update_incomplete_cache_doc(IncompleteCacheDoc, BucketId, UrlId),
					update_cache({add_index, UpdatedCacheDoc}, State);

				false ->
					lager:debug("Index for word id: ~p not found in db. Creating new index.", [WordId]),
					update_cache({add_index, create_new_cache_doc(WordId, UrlId)}, State)
			end;
		
		IndexData ->
			UpdatedIndexData = update_index_data(IndexData, UrlId),
			update_cache({update_index_data, WordId, UpdatedIndexData}, State)
			
	end.


spawn_delayed_add_index(Word, UrlId, State) ->
	spawn(?MODULE, delayed_add_index, [Word, UrlId, get_state_value(retry_delay, State), self()]).


delayed_add_index(Word, UrlId, SleepTime, ServerPid) ->
	timer:sleep(SleepTime),
	lager:debug("Retrying to add index for word: ~s with url id: ~p", [Word, UrlId]),
	persistence_server:retry_add_index(ServerPid, Word, UrlId).


%%
%% Index obtaining helper functions.
%%
get_index_data_from_cache(WordId, State) ->
	{ok, IndexData} = cache_server:get_index_data(get_cache_server_name(index, State), WordId),
	IndexData.


get_index_from_db(BucketId, WordId) ->
	case indexdb_functions:get_index(BucketId, WordId) of
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
			{UrlIdList, UrlIdListSize};
			
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
create_new_cache_doc(WordId, UrlId) ->
	UrlIdList = [UrlId],
	UrlIdListSize = 1,
	OldBucketId = unspec,
	InitUrlIdListSize = unspec,
	{WordId, UrlIdList, UrlIdListSize, OldBucketId, InitUrlIdListSize}.


update_cache({update_index_data, WordId, IndexData}, State) ->
	case cache_server:update_index_data(get_cache_server_name(index, State), WordId, IndexData) of
		ok ->
			lager:debug("Index data: ~w updated for word id: ~p.", [IndexData, WordId]),
			ok;
		
		{ok, full} ->
			lager:debug("Cache is full. Cleaning."),
			handle_old_indices(State)
	end;

update_cache({add_index, CacheDoc}, State) ->
	case cache_server:add_index(get_cache_server_name(index, State), CacheDoc) of
		ok ->
			lager:debug("Cache doc: ~w added to cache.", [CacheDoc]),
			ok;
		
		{ok, full} ->
			lager:debug("Cache is full. Cleaning."),
			handle_old_indices(State)
	end.


%%
%% Old indicies handling helper functions.
%% 
handle_old_indices(State) ->
	case cache_server:retrieve_all_indicies(get_cache_server_name(index, State)) of
		{ok, []} ->
			ok;
		
		{ok, CacheDocList} ->
			{ok, BucketId} = id_server:get_bucket_id(),
			Counters = {0, 0},
			Lists = {[], [], []},
			handle_old_indicies(CacheDocList, BucketId, Counters, Lists, State)
	end.


handle_old_indicies([CacheDoc | T], BucketId, Counters, Lists, State) ->
	{WordId, UrlIdList, UrlIdListSize, OldBucketId, InitUrlIdListSize} = CacheDoc,
	{WordCnt, UrlCnt} = Counters,
	{IncompleteCacheDocList, WordIdToUpdateList, WordIdToFreezeList} = Lists,
	enqueue_index_to_delete(OldBucketId, InitUrlIdListSize, WordId, State),
	IncompleteCacheDoc = {WordId, UrlIdList, UrlIdListSize},
	NewCounters = {WordCnt + 1, UrlCnt + UrlIdListSize},
	case is_url_id_list_size_max(UrlIdListSize, State) of
		true ->
			NewLists = {[IncompleteCacheDoc | IncompleteCacheDocList], WordIdToUpdateList, [WordId | WordIdToFreezeList]},
			handle_old_indicies(T, BucketId, NewCounters, NewLists, State);
		
		false ->
			NewLists = {[ IncompleteCacheDoc | IncompleteCacheDocList], [WordId | WordIdToUpdateList], WordIdToFreezeList},
			handle_old_indicies(T, BucketId, NewCounters, NewLists, State)
	end;

handle_old_indicies([], BucketId, Counters, Lists, _State) ->
	{WordCnt, UrlCnt} = Counters,
	{IncompleteCacheDocList, WordIdToUpdateList, WordIdToFreezeList} = Lists,
	save_indicies_as_bucket(BucketId, WordCnt, UrlCnt, IncompleteCacheDocList, WordIdToUpdateList),
	freeze_indicies_in_bucket(WordIdToFreezeList, BucketId).

			
enqueue_index_to_delete(unspec, unspec, _WordId, _State) ->
	ok;

enqueue_index_to_delete(OldBucketId, InitUrlIdListSize, WordId, State) ->
	db_cleaner_server:add_index_to_delete(get_cache_server_name(index, State), OldBucketId, InitUrlIdListSize, WordId).


save_indicies_as_bucket(BucketId, WordCnt, UrlCnt, IncompleteCacheDocList, WordIdToUpdateList) ->
	indexdb_functions:save_indicies(BucketId, WordCnt, UrlCnt, IncompleteCacheDocList),
	wordsdb_functions:update_active_bucket_id(WordIdToUpdateList, BucketId).


freeze_indicies_in_bucket([], _BucketId) ->
	ok;

freeze_indicies_in_bucket(WordIdToFreezeList, BucketId) ->
	wordsdb_functions:freeze_bucket_id(WordIdToFreezeList, BucketId),
	wordsdb_functions:update_active_bucket_id(WordIdToFreezeList, unspec).

	
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
get_cache_server_name(index, State) ->
	get_state_value(index_cache_server_name, State);

get_cache_server_name(cleaner, State) ->
	get_state_value(cleaner_cache_server_name, State).

%%
%% State handling helper functions.
%% 
get_state_value(max_cache_doc_size, {{max_cache_doc_size, Value}, _, _, _}) ->
	Value;

get_state_value(retry_delay, {_, {retry_delay, Value}, _, _}) ->
	Value;

get_state_value(index_cache_server_name, {_, _, {index_cache_server_name, Value}, _}) ->
	Value;

get_state_value(cleaner_cache_server_name, {_, _, _, {cleaner_cache_server_name, Value}}) ->
	Value.
