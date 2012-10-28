-module(persistence_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_index/2]).

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

add_index(Word, UrlId) ->
	gen_server:cast(?SERVER, {add_index, Word, UrlId}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_PersistenceCfg) ->
	State = [],
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({add_index, Word, UrlId}, State) ->
	{WordId, BucketId} = add_word(Word),
	add_index(WordId, BucketId, UrlId),
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

add_word(Word) ->
	{ok, WordData} = wordsdb_server:add_word(Word),
	WordData.


add_index(WordId, BucketId, UrlId) ->
	case retrieve_index({from_cache}, WordId) of
		index_not_found ->
			case is_bucket_in_db(BucketId) of
				true ->
					Index = retrieve_index({from_db, BucketId}, WordId),
					UpdatedIndex = update_index(Index, UrlId),
					update_cache(UpdatedIndex);

				false ->
					Index = create_index_doc(WordId, [UrlId], 1, BucketId),
					update_cache(Index)
			end;
		
		Index ->
			UpdatedIndex = update_index(Index, UrlId),
			update_cache(UpdatedIndex)
			
	end.


retrieve_index({from_cache}, WordId) ->
	{ok, Index} = cache_server:retrieve_index(WordId),
	Index;
retrieve_index({from_db, BucketId}, WordId) ->
	case indexdb_server:get_index(BucketId, WordId) of
		{ok, no_entry_for_word} ->
			to_do;
		
		{ok, no_bucket} ->
			to_do;
		
		{ok, {word_id, WordId, urls_ids, UrlsIdsList}} -> 
			%% Return index in the format that is used in cache
			{WordId, UrlsIdsList, length(UrlsIdsList), BucketId, length(UrlsIdsList)}
	end.


update_index({WordId, UrlsIdsList, UrlsIdsListSize, BucketId, IndexSizeInBucket}, UrlId) ->
	case update_urls_ids_list(UrlsIdsList, UrlId) of
		{not_updated, _} ->
			{WordId, UrlsIdsList, UrlsIdsListSize, BucketId, IndexSizeInBucket};
			
		{updated, UpdatedUrlsIdsList} -> 
			{WordId, UpdatedUrlsIdsList, UrlsIdsListSize + 1, BucketId, IndexSizeInBucket}
	end.


update_cache({WordId, UrlsIdsList, UrlsIdsListSize, BucketId, IndexSizeInBucket}) ->
	case cache_server:add_index(WordId, UrlsIdsList, UrlsIdsListSize, BucketId, IndexSizeInBucket) of
		{ok, cache_ok} ->
			ok;
		
		{ok, cache_full} ->
			{ok, OldIndicies} = cache_server:retrieve_old_indicies(),
			NewBucketId = id_server:get_id(),
			%% Format cache to be suitable for db, update words' ids
			%% Bucket size indicates the number of urls ids in the index
			{CustomizedOldIndicies, NewBucketSize, WordsIdsToUpdate} = handle_old_indicies(OldIndicies),
			indexdb_server:add_indicies(CustomizedOldIndicies, NewBucketSize, NewBucketId),
			wordsdb_server:set_words_bucket_id(NewBucketId, WordsIdsToUpdate),
			%% Set new bucket in db cleaner
			db_cleaner_server:set_bucket(NewBucketId, NewBucketSize),
			%% Enqueue old indicies to clean
			enqueue_indicies_to_delete(OldIndicies)
	end.


create_index_doc(WordId, UrlsIdsList, UrlsIdsListSize, BucketId) ->
	%% Create index doc is invoked only if the index exists neither in cache nor in the db
	%% So we don't know index size in bucket, so we set it to 0
	{WordId, UrlsIdsList, UrlsIdsListSize, BucketId, unspec}.


update_urls_ids_list([], UrlId) ->
	{updated, [UrlId]};
update_urls_ids_list([ H | T ], UrlId) ->
	%% Decending order.
	if 
		UrlId > H ->
			{updated, [ UrlId | [ H | T ] ]};
		UrlId < H ->
			{Status, List} = update_urls_ids_list(T, UrlId),
			{Status, [ H | List ]};
		UrlId == H ->
			{not_updated, [ H | T ]}
	end.


handle_old_indicies(OldIndicies) ->
	%% Customize for db, count words ids, update bucket ids for words
	customize_count_update_words(OldIndicies, [], 0, []).


customize_count_update_words([], CustomizedIndicies, BucketSize, WordsIdsToUpdate) ->
	{CustomizedIndicies, BucketSize, WordsIdsToUpdate};
customize_count_update_words([ {WordId, UrlsIdsList, UrlsIdsListSize, _, _} | OldIndicies], CustomizedIndices, BucketSize, WordsIdsToUpdate) ->
	%% Customize index to be sutiable for the db.
	CustomizedIndex = customize_old_index(WordId, UrlsIdsList),
	%% Index size indicates size of the urls' ids list size.
	IndexSize = UrlsIdsListSize,
	customize_count_update_words(OldIndicies, [CustomizedIndex | CustomizedIndices], BucketSize + IndexSize, [ WordId | WordsIdsToUpdate]).


enqueue_indicies_to_delete([]) ->
	ok;
enqueue_indicies_to_delete([ {WordId, _, _IndexSize, OldBucketId, IndexSizeInBucket} | OldIndicies ]) ->
	case is_bucket_in_db(OldBucketId) of 
		true -> 
			db_cleaner_server:add_entry(OldBucketId, IndexSizeInBucket, WordId),
			enqueue_indicies_to_delete(OldIndicies);
		false ->
			enqueue_indicies_to_delete(OldIndicies)
	end.


customize_old_index(WordId, UrlsIdsList) ->
	{word_id, WordId, urls_ids, UrlsIdsList}.


is_bucket_in_db(BucketId) ->
	if
		BucketId == 0 ->
			false;
		BucketId =/= 0 ->
			true
	end.
