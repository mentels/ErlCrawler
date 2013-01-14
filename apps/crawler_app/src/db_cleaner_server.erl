-module(db_cleaner_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, set_bucket/2, add_index_to_delete/3]).
-export([get_state/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(DbCleanerCfg) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, DbCleanerCfg, []).

set_bucket(BucketId, UrlIdCnt) ->
	%% UrlIdCnt refers to the number of urls' ids that the bucket contains.
	gen_server:cast(?SERVER, {set_bucket, BucketId, UrlIdCnt}).

add_index_to_delete(BucketId, UrlIdListSize, WordId) ->
	%% UrlIdListSize refers to number of urls' ids persited in the bucket for given word id.
	gen_server:cast(?SERVER, {add_index_to_delete, BucketId, UrlIdListSize, WordId}).

get_state() ->
	gen_server:call(?SERVER, {get_state}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(DbCleanerCfg) ->
	[MaxCacheSizeCfg, MaxUnusedUrlIdCntPercentageCfg] = DbCleanerCfg,
    
	%% cache hold the list of tuples of format: {BucketId, UrlIdCnt, UnusedUrlIdCnt, WordIdList, WordIdListSize}
	%% size refers to summary sizes of WordIdList of each entry; it is the sum of WordIdListSize of each entry
	State = {{cache, []}, {size, 0}, MaxCacheSizeCfg, MaxUnusedUrlIdCntPercentageCfg},
    {ok, State}.


handle_call({get_state}, _From, State) ->
	{reply, State, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({set_bucket, BucketId, UrlIdCnt}, State) ->
	UpdatedState = update_state({add_entry, BucketId, UrlIdCnt}, State),
	{noreply, UpdatedState};

handle_cast({add_index_to_delete, BucketId, UrlIdListSize, WordId}, State) ->
	CacheEntry = get_cache_entry(BucketId, State),
	case is_bucket_ready_to_clean(CacheEntry, UrlIdListSize, State) of
		true ->
			lager:debug("Bucket id: ~p is ready to clean.", [BucketId]),
			WordIdListSize = get_cache_entry_value(word_id_list_size, CacheEntry),
			case calculate_url_id_cnt(CacheEntry, UrlIdListSize) of
				0 -> 
					clean_indicies(BucketId, bucket_empty),
					{noreply, update_state({delete_entry, BucketId, WordIdListSize}, State)};

				NewUrlIdCnt ->
					WordIdList = [WordId | get_cache_entry_value(word_id_list, CacheEntry) ],
					clean_indicies(BucketId, {WordIdList, WordIdListSize + 1, NewUrlIdCnt}),
					{noreply, update_state({reset_entry, BucketId, WordIdListSize, NewUrlIdCnt}, State)}
			end;
		
		false ->
			lager:debug("Bucket id: ~p is not ready to clean.", [BucketId]),
			UpdatedState = update_state({update_entry, BucketId, UrlIdListSize, WordId}, State),
			case is_cache_full(State) of
				true ->
					lager:debug("Cache is full."),
					{noreply, update_state({clean_cache}, State)};
				false ->
					lager:debug("Cache is not full."),
					{noreply, UpdatedState}
			end
	end;

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


%%
%% Cache entry heleper functions.
%%
create_cache_entry(BucketId, UrlIdCnt) ->
	UnusedUrlIdCnt = 0,
	WordIdList = [],
	WordIdListSize = 0,
	{BucketId, UrlIdCnt, UnusedUrlIdCnt, WordIdList, WordIdListSize}.


get_cache_entry(BucketId, State) ->
	Cache = get_state_value(cache, State),
	lists:keyfind(BucketId, 1, Cache).


get_cache_entry_value(word_id_list, {_, _, _, WordIdList, _}) ->
	WordIdList;

get_cache_entry_value(word_id_list_size, {_, _, _, _, WordIdListSize}) ->
	WordIdListSize.
  

update_cache_entry(UrlIdListSize, WordId, CacheEntry) ->
	{BucketId, UrlIdCnt, UnusedUrlIdCnt, WordIdList, WordIdListSize} = CacheEntry,
	{BucketId, UrlIdCnt, UnusedUrlIdCnt + UrlIdListSize, [WordId | WordIdList], WordIdListSize + 1}.


%%
%% Higher level state handling functions.
%%  
update_state({add_entry, BucketId, UrlIdCnt}, State) ->
	Cache = get_state_value(cache, State),
	NewCacheEntry = create_cache_entry(BucketId, UrlIdCnt),
	update_state_value({cache, [ NewCacheEntry | Cache ]}, State);

update_state({delete_entry, BucketId, WordIdListSize}, State) ->
	{Cache, Size} = get_state_value(cache_and_size, State),
	UpdatedCache = lists:keydelete(BucketId, 1, Cache),
	update_state_value({cache_and_size, UpdatedCache, Size - WordIdListSize}, State);

update_state({reset_entry, BucketId, WordIdListSize, NewUrlIdCnt}, State) ->
	{Cache, Size} = get_state_value(cache_and_size, State),
	UpdatedCache = lists:keydelete(BucketId, 1, Cache),
	NewCacheEntry = create_cache_entry(BucketId, NewUrlIdCnt),
	update_state_value({cache_and_size, [ NewCacheEntry | UpdatedCache], Size - WordIdListSize}, State);

update_state({update_entry, BucketId, UrlIdListSize, WordId}, State) ->
	{Cache, Size} = get_state_value(cache_and_size, State),
	CacheEntry = get_cache_entry(BucketId, State),
	UpdatedCacheEntry = update_cache_entry(UrlIdListSize, WordId, CacheEntry),
	UpdatedCache = lists:keyreplace(BucketId, 1, Cache, UpdatedCacheEntry),
	update_state_value({cache_and_size, UpdatedCache, Size + 1}, State);

update_state({clean_cache}, State) ->
	Cache = get_state_value(cache, State),
	UpdatedCache = clean_cache(Cache, []),
	update_state_value({cache_and_size, UpdatedCache, 0}, State).
	

%%	
%% Cleaning helper functions.	
%%	
clean_indicies(BucketId, bucket_empty) ->
	indexdb_server:delete_bucket(BucketId);

clean_indicies(BucketId, {WordIdList, WordIdListSize, NewUrlIdCnt}) ->
	WordCntDiff = - WordIdListSize,
	indexdb_server:delete_indicies(BucketId, WordIdList, WordCntDiff, NewUrlIdCnt).
	
	
clean_cache([], UpdatedCache) ->
	UpdatedCache;

clean_cache([ {BucketId, UrlIdCnt, UnusedUrlIdCnt, WordIdList, WordIdListSize} | T ], UpdatedCache) ->
	NewUrlIdCnt = UrlIdCnt - UnusedUrlIdCnt,
	clean_indicies(BucketId, {WordIdList, WordIdListSize, NewUrlIdCnt}),
	clean_cache(T, [ {BucketId, NewUrlIdCnt, 0, [], 0} | UpdatedCache ]).


%%
%% Calculation helper functions.
%%
calculate_url_id_cnt({_, UrlIdCnt, UnusedUrlIdCnt, _, _}, UrlIdListSize) ->
	%% Calculate new size of the bucket after cleaning.
	UrlIdCnt - (UnusedUrlIdCnt + UrlIdListSize).


is_bucket_ready_to_clean({_, UrlIdCnt, UnusedUrlIdCnt, _, _}, UrlIdListSize, State) ->
	%% UrlIdListSize referes to the number of urls' ids in the index to be deleted.
	UnusedUrlIdRatio = get_state_value(unused_url_id_ratio, State),
	if
		((UnusedUrlIdCnt + UrlIdListSize) / UrlIdCnt) * 100 >= UnusedUrlIdRatio ->
			true;
		true ->
			false
	end.


is_cache_full(State) ->
	MaxSize = get_state_value(max_size, State),
	Size = get_state_value(size, State),
	if
		Size == MaxSize ->
			true;
		true ->
			false
	end.

%%
%% State handling helper functions.
%%
get_state_value(cache_and_size, {{cache, Cache}, {size, Size}, _, _}) ->
	{Cache, Size};

get_state_value(cache, {{cache, Cache}, _, _, _}) ->
	Cache;

get_state_value(size, {_, {size, Size}, _, _}) ->
	Size;

get_state_value(max_size, {_, _, {max_word_id_cnt, Value}, _}) ->
	Value;

get_state_value(unused_url_id_ratio, {_, _, _, {max_unused_url_id_cnt_percentage, Value}}) ->
	Value.


update_state_value({cache_and_size, NewCache, NewSize}, {_, _, Cfg1, Cfg2}) ->
	lager:debug("Cache updated: ~w; cache size updated: ~p", [NewCache, NewSize]),
	{{cache, NewCache}, {size, NewSize}, Cfg1, Cfg2};

update_state_value({cache, NewCache}, {_, Size, Cfg1, Cfg2}) ->
	lager:debug("Cache updated: ~w", [NewCache]),
	{{cache, NewCache}, Size, Cfg1, Cfg2}.

