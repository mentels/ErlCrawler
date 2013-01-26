-module(indexdb_functions).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([save_indicies/5, get_index/3, get_url_cnt/2, delete_indicies/5, delete_bucket/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

save_indicies(BucketId, WordCnt, UrlCnt, IncompleCacheDocList, ConnManagerServerName) ->
	save_indicies_internal(BucketId, WordCnt, UrlCnt, IncompleCacheDocList, ConnManagerServerName).


get_index(BucketId, WordId, ConnManagerServerName) ->
	get_index_internal(BucketId, WordId, ConnManagerServerName).


get_url_cnt(BucketId, ConnManagerServerName) ->
	get_url_cnt_internal(BucketId, ConnManagerServerName).


delete_indicies(BucketId, WordIdList, WordCntDiff, NewUrlCnt, ConnManagerServerName) ->
	delete_indicies_internal(BucketId, WordIdList, WordCntDiff, NewUrlCnt, ConnManagerServerName).


delete_bucket(BucketId, ConnManagerServerName) ->
	delete_bucket_internal(BucketId, ConnManagerServerName).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

save_indicies_internal(BucketId, WordCnt, UrlCnt, IncompleteCacheDocList, ConnManagerServerName) ->
	DbIndexDocList = convert_incomplete_cache_docs_to_db_index_docs(IncompleteCacheDocList, []),
	DbBucketDoc = {'_id', BucketId, word_cnt, WordCnt, url_cnt, UrlCnt, indicies, DbIndexDocList},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(ConnManagerServerName, index),
	db_helper:perform_action({insert, DbBucketDoc}, ConnCfg).


get_index_internal(BucketId, WordId, ConnManagerServerName) ->
	SelectorDoc = {'_id', BucketId}, 
	ProjectionDoc = {indicies, 1, '_id', 0},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(ConnManagerServerName, index),
	case db_helper:perform_action({find_one, SelectorDoc, ProjectionDoc}, ConnCfg) of
		{ok, {{indicies, DbIndexDocList}}} ->
			case lists:keyfind(WordId, 2, DbIndexDocList) of
				false -> 
					{ok, no_word};
				DbIndexDoc ->
					IncompleteCacheDoc = convert_db_doc_to_incomplete_cache_doc(DbIndexDoc),
					{ok, IncompleteCacheDoc}
			end;

		{ok, {}} ->
			{ok, no_bucket}
	end.
	

get_url_cnt_internal(BucketId, ConnManagerServerName) ->
	SelectorDoc = {'_id', BucketId}, 
	ProjectionDoc = {url_cnt, 1, '_id', 0},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(ConnManagerServerName, index),
	case db_helper:perform_action({find_one, SelectorDoc, ProjectionDoc}, ConnCfg) of
		{ok, {{url_cnt, UrlIdCnt}}} ->
			{ok, UrlIdCnt};

		{ok, {}} ->
			{ok, no_bucket}
	end.

	
delete_indicies_internal(BucketId, WordIdList, WordCntDiff, NewUrlCnt, ConnManagerServerName) ->
	SelectorDoc = {'_id', BucketId},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(ConnManagerServerName, index),
	
	%% Delete from given bucket those entries that are related to word's ids 
	%% contained on the WordIdList. 
	IndiciesModifierDoc = { bson:utf8("$pull"), {indicies, {word_id, { bson:utf8("$in"), WordIdList}}}},
	db_helper:perform_spawned_action({modify, SelectorDoc, IndiciesModifierDoc}, ConnCfg),
	
	%% Update words' ids counter.
	WordCntModifierDoc = { bson:utf8("$inc"), {word_cnt, WordCntDiff} },
	db_helper:perform_action({modify, SelectorDoc, WordCntModifierDoc}, ConnCfg),

	%% Update urls' ids counter.
	UrlCntModifierDoc = { bson:utf8("$set"), {url_cnt, NewUrlCnt} },
	db_helper:perform_action({modify, SelectorDoc, UrlCntModifierDoc}, ConnCfg).


delete_bucket_internal(BucketId, ConnManagerServerName) ->
	SelectorDoc = {'_id', BucketId},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(ConnManagerServerName, index),
	db_helper:perform_spawned_action({delete, SelectorDoc}, ConnCfg).

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
