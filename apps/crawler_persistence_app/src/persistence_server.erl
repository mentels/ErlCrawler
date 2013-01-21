-module(persistence_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_index/2, prepare_to_stop/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(PersistenceCfg) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, PersistenceCfg, []).

prepare_to_stop() ->
	gen_server:call(?SERVER, {prepare_to_stop}).

add_index(Word, UrlId) ->
	gen_server:cast(?SERVER, {add_index, Word, UrlId}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(PersistenceCfg) ->
	process_flag(trap_exit, true),
	[MaxCacheDocSizeCfg] = PersistenceCfg, 
	State = MaxCacheDocSizeCfg,
    {ok, State}.


handle_call({prepare_to_stop}, _From, State) ->
	lager:debug("Cleaning persistence subsytem before stopping..."),
	{ok, CacheDocList} = cache_server:retrieve_all_indicies(),
	handle_old_indices(CacheDocList, State),
	db_cleaner_server:flush_cache(),
	{reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({add_index, Word, UrlId}, State) ->
	{WordId, BucketId} = add_word(Word),
	lager:debug("Adding index: word id: ~p; bucket id: ~p; url id: ~p", [WordId, BucketId, UrlId]),
	add_index(WordId, BucketId, UrlId, State),
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

add_word(Word) ->
	{ok, WordData} = wordsdb_functions:save_word(Word),
	WordData.


add_index(WordId, BucketId, UrlId, State) ->
	case retrieve_index({from_cache}, WordId) of
		index_not_found ->
			lager:debug("Index for word id: ~p not found in cache.", [WordId]),
			case is_bucket_in_db(BucketId) of
				true ->
					IncompleteCacheDoc = retrieve_index({from_db, BucketId}, WordId),
					UpdatedCacheDoc = update_incomplete_cache_doc(IncompleteCacheDoc, BucketId, UrlId),
					update_cache(UpdatedCacheDoc, State);

				false ->
					lager:debug("Index for word id: ~p not found in db. Creating new index.", [WordId]),
					update_cache(create_new_cache_doc(WordId, UrlId), State)
			end;
		
		unexpected ->
			to_do;
		
		CacheDoc ->
			UpdatedCacheDoc = update_cache_doc(CacheDoc, UrlId),
			update_cache(UpdatedCacheDoc, State)
			
	end.


%%
%% Index retrieveing helper functions.
%%
retrieve_index({from_cache}, WordId) ->
	{ok, Result} = cache_server:retrieve_index(WordId),
	Result;


retrieve_index({from_db, BucketId}, WordId) ->
	case indexdb_functions:get_index(BucketId, WordId) of
		{ok, no_word} ->
			lager:error("No word id: ~p found in bucket id: ~p", [WordId, BucketId]),
			unexpected;
		
		{ok, no_bucket} ->
			lager:error("No bucket id found: ~p", [BucketId]),
			unexptected;
		
		{ok, IncompleteCacheDoc} -> 
			IncompleteCacheDoc
	end.


%%
%% Cache doc updating helper functions.
%%
update_cache_doc({WordId, UrlIdList, UrlIdListSize, OldBucketId, InitUrlIdListSize}, UrlId) ->
	case update_url_id_list(UrlIdList, UrlId) of
		{not_updated, _} ->
			{WordId, UrlIdList, UrlIdListSize, OldBucketId, InitUrlIdListSize};
			
		{updated, UpdatedUrlIdList} -> 
			{WordId, UpdatedUrlIdList, UrlIdListSize + 1, OldBucketId, InitUrlIdListSize}
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


update_cache(CacheDoc, State) ->
	case cache_server:add_index(CacheDoc) of
		ok ->
			lager:debug("Cache doc: ~w added to cache.", [CacheDoc]),
			ok;
		
		{ok, full} ->
			lager:debug("Cache is full. Cleaning."),
			{ok, CacheDocList} = cache_server:retrieve_old_indicies(),
			handle_old_indices(CacheDocList, State)
	end.


%%
%% Old indicies handling helper functions.
%% 
handle_old_indices([], _State) ->
	ok;

handle_old_indices(CacheDocList, State) ->
	{ok, BucketId} = id_server:get_bucket_id(),
	Counters = {0, 0},
	Lists = {[], [], []},
	handle_old_indicies(CacheDocList, BucketId, Counters, Lists, State).


handle_old_indicies([CacheDoc | T], BucketId, Counters, Lists, State) ->
	{WordId, UrlIdList, UrlIdListSize, OldBucketId, InitUrlIdListSize} = CacheDoc,
	{WordCnt, UrlCnt} = Counters,
	{IncompleteCacheDocList, WordIdToUpdateList, WordIdToFreezeList} = Lists,
	enqueue_index_to_delete(OldBucketId, InitUrlIdListSize, WordId),
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

			
enqueue_index_to_delete(unspec, unspec, _WordId) ->
	ok;

enqueue_index_to_delete(OldBucketId, InitUrlIdListSize, WordId) ->
	db_cleaner_server:add_index_to_delete(OldBucketId, InitUrlIdListSize, WordId).


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
%% State handling helper functions.
%% 
get_state_value(max_cache_doc_size, {max_cache_doc_size, Value}) ->
	Value.
