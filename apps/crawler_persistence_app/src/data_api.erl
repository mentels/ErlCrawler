-module(data_api).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([get_index/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

get_index(Word) ->
%% 	get_word_id_and_bucket_id_list(Word).
	case get_word_id_and_bucket_id_list(Word) of
		no_bucket ->
			[];
		
		no_word ->
			[];
		
		{WordId, BucketIdList} ->
			get_url_id_list(BucketIdList, WordId)
	end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


		
%%
%% Bucket id retrieving helper functions.
%%

get_word_id_and_bucket_id_list(Word) ->
	{ok, WordDbCfg} = application:get_env(crawler_persistence, word_db_cfg),
	[
  		{conn_cfg, ConnCfg},
  		{db, DbName},
		{coll, CollName}
  	] = WordDbCfg,
	{ok, Conn} = mongo:connect(ConnCfg),
	WordIdAndBucketIdList = get_word_id_and_bucket_id_list_from_db(Word, Conn, DbName, CollName),
	mongo:disconnect(Conn),
	WordIdAndBucketIdList.


get_word_id_and_bucket_id_list_from_db(Word, Conn, DbName, CollName) ->
	SelectorDoc = {word, bson:utf8(Word)},
	ProjectionDoc = {'_id', 1, active_bucket_id, 1, frozen_bucket_id, 1},
%% 	db_helper:perform_action({find_one, SelectorDoc, ProjectionDoc}, CollName, DbName, Conn).
	case db_helper:perform_action({find_one, SelectorDoc, ProjectionDoc}, CollName, DbName, Conn) of
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
get_url_id_list(BucketIdList, WordId)->
	{ok, IndexDbCfg} = application:get_env(crawler_persistence, index_db_cfg),
	[
  		{conn_cfg, ConnCfg},
  		{db, DbName},
		{coll, CollName}
  	] = IndexDbCfg,
	{ok, Conn} = mongo:connect(ConnCfg),
	UrlIdList = get_url_id_list_from_db(BucketIdList, WordId, Conn, DbName, CollName),
	mongo:disconnect(Conn),
	UrlIdList.


get_url_id_list_from_db(BucketIdList, WordId, Conn, DbName, CollName) ->
	SelectorDoc = {'_id', { bson:utf8("$in"), BucketIdList}}, 
	ProjectionDoc = {indicies, 1, '_id', 0},
	{ok, Cursor} = db_helper:perform_action({find, SelectorDoc, ProjectionDoc}, CollName, DbName, Conn),
	retrieve_url_id_list_from_list_of_db_index_doc_list(WordId, mongo:rest(Cursor), []).
		

retrieve_url_id_list_from_list_of_db_index_doc_list(_, [], ResultUrlIdList) ->
	ResultUrlIdList;
	
retrieve_url_id_list_from_list_of_db_index_doc_list(WordId, [ {indicies, DbIndexDocList} | T ], ResultUrlIdList) ->
	DbIndexDoc = lists:keyfind(WordId, 2, DbIndexDocList),
	{_, _, _, [UrlIdList, _]} = DbIndexDoc,
	retrieve_url_id_list_from_list_of_db_index_doc_list(WordId, T, UrlIdList ++ ResultUrlIdList).
	