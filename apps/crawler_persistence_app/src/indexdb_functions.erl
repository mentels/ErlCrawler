-module(indexdb_functions).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([save_indicies/4, get_index/2, get_url_cnt/1, delete_indicies/4, delete_bucket/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

save_indicies(BucketId, WordCnt, UrlCnt, IncompleCacheDocList) ->
	save_indicies_internal(BucketId, WordCnt, UrlCnt, IncompleCacheDocList).


get_index(BucketId, WordId) ->
	get_index_internal(BucketId, WordId).


get_url_cnt(BucketId) ->
	get_url_cnt_internal(BucketId).


delete_indicies(BucketId, WordIdList, WordCntDiff, NewUrlCnt) ->
	delete_indicies_internal(BucketId, WordIdList, WordCntDiff, NewUrlCnt).


delete_bucket(BucketId) ->
	delete_bucket_internal(BucketId).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

save_indicies_internal(BucketId, WordCnt, UrlCnt, IncompleteCacheDocList) ->
	DbIndexDocList = convert_incomplete_cache_docs_to_db_index_docs(IncompleteCacheDocList, []),
	DbBucketDoc = {'_id', BucketId, word_cnt, WordCnt, url_cnt, UrlCnt, indicies, DbIndexDocList},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(index),
	db_helper:perform_action({insert, DbBucketDoc}, ConnCfg),
	lager:debug("Db bucket doc saved: ~p", [DbBucketDoc]).


get_index_internal(BucketId, WordId) ->
	SelectorDoc = {'_id', BucketId}, 
	ProjectionDoc = {indicies, 1, '_id', 0},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(index),
	case db_helper:perform_action({find_one, SelectorDoc, ProjectionDoc}, ConnCfg) of
		{ok, {{indicies, DbIndexDocList}}} ->
			case lists:keyfind(WordId, 2, DbIndexDocList) of
				false -> 
					lager:debug("No word id: ~p in bucket id: ~p.", [WordId, BucketId]),
					{ok, no_word};
				DbIndexDoc ->
					IncompleteCacheDoc = convert_db_doc_to_incomplete_cache_doc(DbIndexDoc),
					lager:debug("Incomplete cache doc returned: ~p", [IncompleteCacheDoc]),
					{ok, IncompleteCacheDoc}
			end;

		{ok, {}} ->
			lager:debug("No bucket id: ~p", [BucketId]),
			{ok, no_bucket}
	end.
	

get_url_cnt_internal(BucketId) ->
	SelectorDoc = {'_id', BucketId}, 
	ProjectionDoc = {url_cnt, 1, '_id', 0},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(index),
	case db_helper:perform_action({find_one, SelectorDoc, ProjectionDoc}, ConnCfg) of
		{ok, {{url_cnt, UrlIdCnt}}} ->
			lager:debug("Url cnt: ~p returned for bucket id: ~p", [UrlIdCnt, BucketId]),
			{ok, UrlIdCnt};

		{ok, {}} ->
			lager:debug("No bucket id: ~p", [BucketId]),
			{ok, no_bucket}
	end.

	
delete_indicies_internal(BucketId, WordIdList, WordCntDiff, NewUrlCnt) ->
	SelectorDoc = {'_id', BucketId},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(index),
	
	%% Delete from given bucket those entries that are related to word's ids 
	%% contained on the WordIdList. 
	IndiciesModifierDoc = { bson:utf8("$pull"), {indicies, {word_id, { bson:utf8("$in"), WordIdList}}}},
	db_helper:perform_action({modify, SelectorDoc, IndiciesModifierDoc}, ConnCfg),
	lager:debug("From bucket id: ~w db index docs deleted: ~w", [BucketId, WordIdList]),
	
	%% Update words' ids counter.
	WordCntModifierDoc = { bson:utf8("$inc"), {word_cnt, WordCntDiff} },
	db_helper:perform_action({modify, SelectorDoc, WordCntModifierDoc}, ConnCfg),
	lager:debug("Bucket id: ~w updated with new word diff: ~p", [BucketId, WordCntDiff]),

	%% Update urls' ids counter.
	UrlCntModifierDoc = { bson:utf8("$set"), {url_cnt, NewUrlCnt} },
	db_helper:perform_action({modify, SelectorDoc, UrlCntModifierDoc}, ConnCfg),
	lager:debug("Bucket id: ~w updated with new url cnt: ~p", [BucketId, NewUrlCnt]).


delete_bucket_internal(BucketId) ->
	SelectorDoc = {'_id', BucketId},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(index),
	db_helper:perform_action({delete, SelectorDoc}, ConnCfg),
	lager:debug("Bucket id: ~p deleted from db.", [BucketId]).

%%
%% Data handling herlper functions.
%%
convert_incomplete_cache_docs_to_db_index_docs([], DbIndexDocList) ->
	DbIndexDocList;
convert_incomplete_cache_docs_to_db_index_docs([ IncompleteCacheDoc | IncompleteCacheDocList], DbIndexDocList) ->
	{WordId, UrlIdList, UrlIdListSize} = IncompleteCacheDoc,
	DbIndexDoc = {word_id, WordId, data, [UrlIdList, UrlIdListSize]},
	convert_incomplete_cache_docs_to_db_index_docs(IncompleteCacheDocList, DbIndexDocList ++ [DbIndexDoc]).


convert_db_doc_to_incomplete_cache_doc(DbIndexDoc) ->
	{word_id, WordId, data, [UrlIdList, UrlIdListSize]} = DbIndexDoc,
	{WordId, UrlIdList, UrlIdListSize}.
