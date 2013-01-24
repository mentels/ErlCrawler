-module(data_api).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([get_index/1, get_count/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

get_index(Word) ->
	case get_word_id_and_bucket_id_list(Word) of
		no_bucket ->
			[];
		
		no_word ->
			[];
		
		{WordId, BucketIdList} ->
			get_url_id_list(BucketIdList, WordId)
	end.


get_count(words_coll) ->
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(words),
	{ok, Cnt} = db_helper:perform_action({count, {}}, ConnCfg),
	Cnt;

get_count(index_coll) ->
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(index),
	{ok, Cnt} = db_helper:perform_action({count, {}}, ConnCfg),
	Cnt;

get_count(indexes) ->
	ProjectionDoc = {'_id', 0, url_cnt, 1},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(index),
	{ok, Cursor} = db_helper:perform_action({find, {}, ProjectionDoc}, ConnCfg),
	case mongo:rest(Cursor) of
		[] ->
			empty;
		
		UrlCntList ->
			lists:foldl(fun(X, Sum) ->
								{url_cnt, Cnt} = X,
								Sum + Cnt
						end, 0, UrlCntList)
	end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


		
%%
%% Bucket id retrieving helper functions.
%%
get_word_id_and_bucket_id_list(Word) ->
	SelectorDoc = {word, bson:utf8(Word)},
	ProjectionDoc = {'_id', 1, active_bucket_id, 1, frozen_bucket_id, 1},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(words),
	case db_helper:perform_action({find_one, SelectorDoc, ProjectionDoc}, ConnCfg) of
		{ok, {{'_id', _, active_bucket_id, <<"unspec">>, frozen_bucket_id, []}}} ->
			no_bucket;
		
		{ok, {{'_id', WordId, active_bucket_id, <<"unspec">>, frozen_bucket_id, BucketIdList}}} ->
			{WordId, BucketIdList};
		
		{ok, {{'_id', WordId, active_bucket_id, BucketId, frozen_bucket_id, []}}} ->
			{WordId, [BucketId]};
		
		{ok, {{'_id', WordId, active_bucket_id, BucketId, frozen_bucket_id, BucketIdList}}} ->
			{WordId, [BucketId | BucketIdList]};
		
		{ok, {}} ->
			no_word
	end.

%%%
%%% Index retrieving helper functions.
%%%
get_url_id_list(BucketIdList, WordId) ->
	SelectorDoc = {'_id', { bson:utf8("$in"), BucketIdList}}, 
	ProjectionDoc = {indicies, 1, '_id', 0},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(index),
	{ok, Cursor} = db_helper:perform_action({find, SelectorDoc, ProjectionDoc}, ConnCfg),
	retrieve_url_id_list_from_list_of_db_index_doc_list(WordId, mongo:rest(Cursor), []).
		

retrieve_url_id_list_from_list_of_db_index_doc_list(_, [], ResultUrlIdList) ->
	ResultUrlIdList;
	
retrieve_url_id_list_from_list_of_db_index_doc_list(WordId, [ {indicies, DbIndexDocList} | T ], ResultUrlIdList) ->
	DbIndexDoc = lists:keyfind(WordId, 2, DbIndexDocList),
	{_, _, _, [UrlIdList, _]} = DbIndexDoc,
	retrieve_url_id_list_from_list_of_db_index_doc_list(WordId, T, UrlIdList ++ ResultUrlIdList).
	