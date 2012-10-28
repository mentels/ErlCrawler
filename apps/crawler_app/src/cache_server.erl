-module(cache_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_index/5	, retrieve_index/1, retrieve_old_indicies/0]).
-export([get_state/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(CacheCfg) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, CacheCfg, []).

add_index(WordId, UrlsIdsList, UrlsIdsListSize, BucketId, IndexSizeInBucket) ->
	gen_server:call(?SERVER, {add_index, WordId, UrlsIdsList, UrlsIdsListSize, BucketId, IndexSizeInBucket}).

retrieve_index(WordId) ->
	gen_server:call(?SERVER, {retrieve_index, WordId}).

retrieve_old_indicies() ->
	gen_server:call(?SERVER, {retrieve_old_indicies}).

get_state() ->
	gen_server:call(?SERVER, {get_state}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(CacheCfg) ->
	process_flag(trap_exit, true),
	State = CacheCfg ++ [{cache_size, 0}, {cache, []}, {recently_retrieved_index_size, none}],
    {ok, State}.


handle_call({add_index, WordId, UrlsIdsList, UrlsIdsListSize, BucketId, IndexSizeInBucket}, _From, State) ->
	IndexDoc = create_index_doc(WordId, UrlsIdsList, UrlsIdsListSize, BucketId, IndexSizeInBucket),
	TmpState = update_state({add_index, IndexDoc}, State),
	UpdatedState = update_state({increase_size, UrlsIdsListSize}, TmpState),
	case is_cache_full(UpdatedState) of
		false -> {reply, {ok, cache_ok}, UpdatedState};
		true -> {reply, {ok, cache_full}, UpdatedState}
	end;

handle_call({retrieve_index, WordId}, _From, State) ->
	{RetrievedIndex, TmpState} = update_state({retrieve_index, WordId}, State),
	%% We don't change cache size as we know that the index will be returned to the cache; with new url_id 
	%% or without.
	UpdatedState = update_state({set_recently_retrieved_index_size, RetrievedIndex}, TmpState),
	case RetrievedIndex of
		{} -> 	{reply, {ok, index_not_found}, UpdatedState};
		_ ->	{reply, {ok, RetrievedIndex}, UpdatedState}
	end;

handle_call({retrieve_old_indicies}, _From, State) ->
	%% We count how much urls ids we want to retrieve as cache size refers url's ids from the indicies.
	RemainingUrlsIdsCount = calculate_remaining_urls_ids_count(State),
	{RetrievedOldIndices, TmpState, NewCacheSize} = update_state({retrieve_old_indicies, RemainingUrlsIdsCount}, State),
	UpdatedState = update_state({set_size, NewCacheSize}, TmpState),
	{reply, {ok, RetrievedOldIndices}, UpdatedState};

handle_call({get_state}, _From, State) ->
	{reply, State, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


terminate(shutdown, _State) ->
	io:format("cache_server terminates...~n"),
	ok;

terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

create_index_doc(WordId, UrlsIdsList, UrlsIdsListSize, BucketId, IndexSizeInBucket) ->
	{WordId, UrlsIdsList, UrlsIdsListSize, BucketId, IndexSizeInBucket}.


calculate_remaining_urls_ids_count(State) ->
	CacheSize = get_state_property(cache_size, State),
	PercentageToFlush = get_state_property(percentage_to_flush, State),
	CacheSize - (CacheSize * PercentageToFlush) / 100.	
  

update_state({add_index, IndexDoc}, State) ->
	Cache = get_state_property(cache, State),
	set_state_property({cache, [ IndexDoc | Cache ]}, State);
update_state({increase_size, UrlsIdsListSize}, State) ->
	case get_state_property(recently_retrieved_index_size, State) of
		none ->				%% We didn't retrieved any index recently
							OldSize = get_state_property(cache_size, State),
							set_state_property({cache_size, OldSize + UrlsIdsListSize}, State);
		
		OldIndexSize ->		%% We previously retrieved the index and we wrote down its size
							if
								OldIndexSize /= UrlsIdsListSize ->	
									OldSize = get_state_property(cache_size, State),
									IndexSizeDiff = UrlsIdsListSize - OldIndexSize,
									set_state_property({cache_size, OldSize + IndexSizeDiff}, State);
								true -> 							
									State
							end
	end;
update_state({set_size, NewCacheSize}, State) ->
	set_state_property({cache_size, NewCacheSize}, State);
update_state({retrieve_index, WordId}, State) ->
	Cache = get_state_property(cache, State),
	{RetrievedIndex, UpdatedCache} = update_cache({retrieve_index, WordId, []}, Cache),
	UpdatedState = set_state_property({cache, UpdatedCache}, State),
	{RetrievedIndex, UpdatedState};
update_state({set_recently_retrieved_index_size, RetrievedIndex}, State) ->
	case RetrievedIndex of
		{} ->	set_state_property({set_recently_retrieved_index_size, none}, State);
		_ ->	IndexSize = element(3, RetrievedIndex),
				set_state_property({set_recently_retrieved_index_size, IndexSize}, State)
	end;
update_state({retrieve_old_indicies, RemainingUrlsIdsCount}, State) ->
	Cache = get_state_property(cache, State),
	{RetrievedIndicies, UpdatedCache, UpdatedCacheSize} = update_cache({retrieve_old_indicies, RemainingUrlsIdsCount, 0, []}, Cache),
	UpdatedState = set_state_property({cache, UpdatedCache}, State),
	{RetrievedIndicies, UpdatedState, UpdatedCacheSize}.
	
	
	
get_state_property(cache, State) ->
	{_, Cache} = lists:keyfind(cache, 1, State),
	Cache;
get_state_property(cache_size, State) ->
	{_, Size} = lists:keyfind(cache_size, 1, State),
	Size;
get_state_property(cache_max_size, State) ->
	{_, CacheMaxSize} = lists:keyfind(max_number_of_words_ids_in_cache, 1, State),
	CacheMaxSize;
get_state_property(recently_retrieved_index_size, State) ->
	{_, IndexSize} = lists:keyfind(recently_retrieved_index_size, 1, State),
	IndexSize;
get_state_property(percentage_to_flush, State) ->
	{_, PercentageToFlush} = lists:keyfind(percentage_of_cache_to_flush, 1, State),
	PercentageToFlush.


set_state_property({cache, Cache}, State) ->
	lists:keyreplace(cache, 1, State, {cache, Cache});
set_state_property({cache_size, Size}, State) ->
	lists:keyreplace(cache_size, 1, State, {cache_size, Size});
set_state_property({set_recently_retrieved_index_size, IndexSize}, State) ->
	lists:keyreplace(recently_retrieved_index_size, 1, State, {recently_retrieved_index_size, IndexSize}).
	

update_cache({retrieve_index, _WoridId, ProcessedCache}, []) ->
	{{}, ProcessedCache};
update_cache({retrieve_index, WordId, ProcessedCache}, [ ExaminedIndex | RemainingCache ]) ->
	case is_cache_entry_for_word_id(WordId, ExaminedIndex) of 
		true ->		{ExaminedIndex, ProcessedCache ++ RemainingCache};
		false -> 	update_cache({retrieve_index, WordId, ProcessedCache ++ [ExaminedIndex]}, RemainingCache)
	end;
update_cache({retrieve_old_indicies, RemainingUrlsIdsCount, ProccessedUrlsIdsCount, ProcessedCache}, [ ExaminedIndex | RemainingCache ]) ->
	ExaminedIndexSize = element(3, ExaminedIndex),
	if
		ExaminedIndexSize + ProccessedUrlsIdsCount > RemainingUrlsIdsCount ->
			{[ExaminedIndex] ++ RemainingCache, ProcessedCache, ProccessedUrlsIdsCount};
		true ->
			update_cache({retrieve_old_indicies, RemainingUrlsIdsCount, ProccessedUrlsIdsCount + ExaminedIndexSize, ProcessedCache ++ [ExaminedIndex]}, RemainingCache)			
	end.

is_cache_full(State) ->
	CacheSize = get_state_property(cache_size, State),
	CacheMaxSize = get_state_property(cache_max_size, State),
	if
		CacheSize >= CacheMaxSize ->
			true;
		true ->
			false
	end.

is_cache_entry_for_word_id(WordId, ExaminedIndex) ->
	ExaminedWordId = element(1, ExaminedIndex),
	if
		WordId == ExaminedWordId ->
			true;
		true ->
			false
	end.