-module(db_cleaner_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_entry/3, set_bucket/2]).
-export([get_state/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(CleanerCfg) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, CleanerCfg, []).

add_entry(BucketId, NewEntryUrlsIdsCnt, WordId) ->
	%% NewEntryUrlsIdsCnt refers to the number of urls ids that will be deleted 
	%% along with the index associated with the given word id.
	gen_server:cast(?SERVER, {add_entry, BucketId, NewEntryUrlsIdsCnt, WordId}).

set_bucket(BucketId, UrlsIdsCnt) ->
	%% UrlsIdsCnt refers to the number of urls ids that the bucket contains.
	gen_server:cast(?SERVER, {set_bucket, BucketId, UrlsIdsCnt}).

get_state() ->
	gen_server:call(?SERVER, {get_state}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(CleanerCfg) ->
	process_flag(trap_exit, true),
	State = CleanerCfg ++ [{cache_size, 0}, {cleaner_cache, []}],
    {ok, State}.


handle_call({get_state}, _From, State) ->
	{reply, State, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({add_entry, BucketId, NewEntryUrlsIdsCnt, WordId}, State) ->
	%% UrlsIdsCnt refers to all urls ids in associated with all words ids in the bucket.
	%% UnusedUrlsIdsCnt indicates the number of all urls ids that are associated with words ids that are to be deleted.
	{_BucketId, UrlsIdsCnt, UnusedUrlsIdsCnt, WordsIdsList, WordsCnt} = get_cache_entry(BucketId, State),
	%% Cache size indicates words ids cnt. 
	NewUnusedUrlsIdsCnt = UnusedUrlsIdsCnt + NewEntryUrlsIdsCnt,
	case is_cache_for_bucket_ready_to_clean(UrlsIdsCnt, NewUnusedUrlsIdsCnt, State) of 
		true ->
			NewUrlsIdsCnt = UrlsIdsCnt - NewUnusedUrlsIdsCnt,
			UpdatedState = update_cache_size(get_cache_size(State) - WordsCnt, State),
			clean_indicies(BucketId, WordsIdsList ++ [WordId], NewUrlsIdsCnt),
			io:format("UrlsIdsCnt~w~n", [UrlsIdsCnt]),
			io:format("UnusedUrlsIdsCnt~w~n", [UnusedUrlsIdsCnt]),
			io:format("NewEntryUrlsIdsCnt~w~n", [NewEntryUrlsIdsCnt]),
			io:format("NewUnusedUrlsIdsCnt~w~n", [NewUnusedUrlsIdsCnt]),
			io:format("NewUrlsIdsCnt~w~n", [NewUrlsIdsCnt]),
			if
				NewUrlsIdsCnt == 0 ->
					%% All the urls ids in the bucket has been cleaned.
					UpdatedCache = delete_cache_entry(BucketId, UpdatedState),
					{noreply, update_state(UpdatedCache, UpdatedState)};
				
				NewUrlsIdsCnt =/= 0 ->
					%% There're still some urls ids in the bucket that weren't cleaned.
					UpdatedCacheEntry = {BucketId, NewUrlsIdsCnt, 0, [], 0},
					UpdatedCache = update_cache_entry(UpdatedCacheEntry, UpdatedState),
					{noreply, update_state(UpdatedCache, UpdatedState)}
			end;
					
		false ->	
			UpdatedCacheEntry = {BucketId, UrlsIdsCnt, NewUnusedUrlsIdsCnt, WordsIdsList ++ [WordId], WordsCnt + 1},
			UpdatedCache = update_cache_entry(UpdatedCacheEntry, State),
			UpdatedState = update_cache_size(get_cache_size(State) + 1, State),
			case is_cache_full(UpdatedState) of
				true ->	
					NewCache = clean_cache(UpdatedCache, []),
					NewState = update_cache_size(0, UpdatedState),
					{noreply, update_state(NewCache, NewState)};
						
				false ->	
					{noreply, update_state(UpdatedCache, UpdatedState)}
			end
	end;

handle_cast({set_bucket, BucketId, UrlsIdsCnt}, State) ->
	CacheEntry = {BucketId, UrlsIdsCnt, 0, [], 0},
	UpdatedCache = add_new_cache_entry(CacheEntry, State),
	UpdatedState = update_state(UpdatedCache, State),
	{noreply, UpdatedState};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(shutdown, _State) ->
	io:format("db_cleaner_server terminates...~n"),
	ok;

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_cache_entry(BucketId, [_, _, _, {cleaner_cache, Cache}]) ->
	lists:keyfind(BucketId, 1, Cache).

get_cache_size(State) ->
	{_, Size} = lists:keyfind(cache_size, 1, State),
	Size.

add_new_cache_entry(CacheEntry, [_, _, _, {cleaner_cache, Cache}]) ->
	Cache ++ [CacheEntry].

update_cache_entry(UpdatedCacheEntry, [_, _, _, {cleaner_cache, Cache}]) ->
	BucketId = element(1, UpdatedCacheEntry),
	lists:keyreplace(BucketId, 1, Cache, UpdatedCacheEntry).

delete_cache_entry(BucketId, [_, _, _, {cleaner_cache, Cache}]) ->
	lists:keydelete(BucketId, 1, Cache).

update_cache_size(NewCacheSize, State) ->
	lists:keyreplace(cache_size, 1, State, {cache_size, NewCacheSize}).

update_state(UpdatedCache, State) ->
	lists:keyreplace(cleaner_cache, 1, State, {cleaner_cache, UpdatedCache}).

clean_indicies(BucketId, WordsIdsList, NewBucketSize) ->
	indexdb_server:delete_indicies(BucketId, WordsIdsList, NewBucketSize),
	ok.

clean_cache([], UpdatedCache) ->
	UpdatedCache;
clean_cache([ {BucketId, UrlsIdsCnt, UnusedUrlsIdsCnt, WordsIdsList, _WordsCnt} | T ], UpdatedCache) ->
	NewUrlsIdsCnt = UrlsIdsCnt - UnusedUrlsIdsCnt,
	clean_indicies(BucketId, WordsIdsList, NewUrlsIdsCnt),
	clean_cache(T, [ {BucketId, NewUrlsIdsCnt, 0, [], 0} | UpdatedCache ]).
	
	

is_cache_for_bucket_ready_to_clean(UrlsIdsCnt, UnusedUrlsIdsCnt, State) ->
	MaxPercentage = get_max_percentage_of_unused_bucket_indicies(State),
	if 
		(UnusedUrlsIdsCnt / UrlsIdsCnt) * 100 >= MaxPercentage ->
			true;
		true ->
			false
	end.

is_cache_full(State) ->
	CacheSize = get_cache_size(State),
	MaxCacheSize = get_max_number_of_indicies_in_cache(State),
	if
		CacheSize == MaxCacheSize ->
			true;
		true ->
			false

	end.
get_max_percentage_of_unused_bucket_indicies(State) ->
	{_, Value} = lists:keyfind(max_percentage_of_unused_bucket_indicies, 1, State),
	Value.

get_max_number_of_indicies_in_cache(State) ->
	{_, Value} = lists:keyfind(max_number_of_indicies_in_cache, 1, State),
	Value.