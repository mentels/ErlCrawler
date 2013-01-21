-module(db_cleaner_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_index_to_delete/3, flush_cache/0]).
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

add_index_to_delete(BucketId, UrlIdListSize, WordId) ->
	%% UrlIdListSize refers to number of urls' ids persited in the bucket for given word id.
	gen_server:cast(?SERVER, {add_index_to_delete, BucketId, UrlIdListSize, WordId}).

flush_cache() ->
	gen_server:call(?SERVER, {flush_cache}).

get_state() ->
	gen_server:call(?SERVER, {get_state}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(DbCleanerCfg) ->
	process_flag(trap_exit, true),
	[MaxCacheSizeCfg, MaxUnusedUrlIdCntPercentageCfg] = DbCleanerCfg,
    
	%% cache hold the list of tuples of format: {BucketId, UrlIdCnt, UnusedUrlIdCnt, WordIdList, WordIdListSize}
	%% size refers to summary sizes of WordIdList of each entry; it is the sum of WordIdListSize of each entry
	CacheTabId = ets:new(cleaner_cache, [set, {keypos, 1}, private, named_table]),
	State = {{cache_tab_id, CacheTabId}, {size, 0}, MaxCacheSizeCfg, MaxUnusedUrlIdCntPercentageCfg},
    {ok, State}.


handle_call({get_state}, _From, State) ->
	{reply, State, State};

handle_call({flush_cache}, _From, State) ->
	{reply, ok, update_state({clean_cache}, State)};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({add_index_to_delete, BucketId, UrlIdListSize, WordId}, State) ->
	CacheEntry = get_cache_entry(BucketId, State),
	WordIdListSize = get_cache_entry_value(word_id_list_size, CacheEntry),
	case is_bucket_ready_to_clean(CacheEntry, UrlIdListSize, State) of
		true ->
			lager:debug("Bucket id: ~p is ready to clean.", [BucketId]),
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
			NewUnusedUrlIdCnt = get_cache_entry_value(unused_url_id_cnt, CacheEntry) + UrlIdListSize,
			NewWordIdList = [WordId | get_cache_entry_value(word_id_list, CacheEntry) ],
			UpdatedState = update_state({update_entry, BucketId, NewUnusedUrlIdCnt, NewWordIdList, WordIdListSize + 1}, State),
			case is_cache_full(UpdatedState) of
				true ->
					lager:debug("Cache is full."),
					{noreply, update_state({clean_cache}, UpdatedState)};
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
	lager:debug("Db cleaner server terminating for shutdown reason."),
	ok;

terminate(Reason, _State) ->
	lager:debug("Db cleaner server terminating for reason: ~p", [Reason]),
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
	CacheTabId = get_state_value(cache_tab_id, State),
	case ets:lookup(CacheTabId, BucketId) of
		[] ->
			lager:debug("Cache entry for bucket id: ~p not found.", [BucketId]),
			{ok, UrlIdCnt} = indexdb_functions:get_url_cnt(BucketId),
			CacheEntry = create_cache_entry(BucketId, UrlIdCnt),
			ets:insert(CacheTabId, CacheEntry),
			CacheEntry;
		
		[CacheEntry] ->
			lager:debug("Cache entry for bucket id: ~p found.", [BucketId]),
			CacheEntry
	end.
	

get_cache_entry_value(unused_url_id_cnt, {_, _, UnusedUrlIdCnt, _, _}) ->
	UnusedUrlIdCnt;

get_cache_entry_value(word_id_list, {_, _, _, WordIdList, _}) ->
	WordIdList;

get_cache_entry_value(word_id_list_size, {_, _, _, _, WordIdListSize}) ->
	WordIdListSize.


%%
%% Higher level state handling functions.
%%  
update_state({delete_entry, BucketId, WordIdListSize}, State) ->
	{CacheTabId, Size} = get_state_value(cache_tab_id_and_size, State),
	ets:delete(CacheTabId, BucketId),
	update_state_value({size, Size - WordIdListSize}, State);

update_state({reset_entry, BucketId, WordIdListSize, NewUrlIdCnt}, State) ->
	{CacheTabId, Size} = get_state_value(cache_tab_id_and_size, State),
	ets:insert(CacheTabId, create_cache_entry(BucketId, NewUrlIdCnt)),
	update_state_value({size, Size - WordIdListSize}, State);

update_state({update_entry, BucketId, NewUnusedUrlIdCnt, NewWordIdList, NewWordIdListSize}, State) ->
	{CacheTabId, Size} = get_state_value(cache_tab_id_and_size, State),
	ets:update_element(CacheTabId, BucketId, [{3, NewUnusedUrlIdCnt}, {4, NewWordIdList}, {5, NewWordIdListSize}]),
	update_state_value({size, Size + 1}, State);

update_state({clean_cache}, State) ->
	CacheTabId = get_state_value(cache_tab_id, State),
	clean_cache(ets:match_object(CacheTabId, '$1')),
	ets:delete_all_objects(CacheTabId),
	update_state_value({size, 0}, State).
	

%%	
%% Cleaning helper functions.	
%%	
clean_indicies(BucketId, bucket_empty) ->
	indexdb_functions:delete_bucket(BucketId);

clean_indicies(BucketId, {WordIdList, WordIdListSize, NewUrlIdCnt}) ->
	WordCntDiff = - WordIdListSize,
	indexdb_functions:delete_indicies(BucketId, WordIdList, WordCntDiff, NewUrlIdCnt).
	
	
clean_cache([]) ->
	ok;

clean_cache([ {BucketId, UrlIdCnt, UnusedUrlIdCnt, WordIdList, WordIdListSize} | T ]) ->
	NewUrlIdCnt = UrlIdCnt - UnusedUrlIdCnt,
	clean_indicies(BucketId, {WordIdList, WordIdListSize, NewUrlIdCnt}),
	clean_cache(T).


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
get_state_value(cache_tab_id_and_size, {{cache_tab_id, CacheTabId}, {size, Size}, _, _}) ->
	{CacheTabId, Size};

get_state_value(cache_tab_id, {{cache_tab_id, CacheTabId}, _, _, _}) ->
	CacheTabId;

get_state_value(size, {_, {size, Size}, _, _}) ->
	Size;

get_state_value(max_size, {_, _, {max_word_id_cnt, Value}, _}) ->
	Value;

get_state_value(unused_url_id_ratio, {_, _, _, {max_unused_url_id_cnt_percentage, Value}}) ->
	Value.


update_state_value({size, NewSize}, {CacheCfg, _, Cfg1, Cfg2}) ->
	lager:debug("Cache size updated: ~p", [NewSize]),
	{CacheCfg, {size, NewSize}, Cfg1, Cfg2}.
