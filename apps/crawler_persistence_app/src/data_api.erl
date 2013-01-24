-module(data_api).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([get_index/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

get_index(Word) ->
	lager:debug("Request for url id list for word: ~s", [Word]),
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
	SelectorDoc = {word, bson:utf8(Word)},
	ProjectionDoc = {'_id', 1, active_bucket_id, 1, frozen_bucket_id, 1},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(conn_manager_server_master, words),
	case db_helper:perform_action({find_one, SelectorDoc, ProjectionDoc}, ConnCfg) of
		{ok, {{'_id', _, active_bucket_id, <<"unspec">>, frozen_bucket_id, []}}} ->
			lager:debug("Word: ~s has no bucket nor frozen buckets assigned.", [Word]),
			no_bucket;
		
		{ok, {{'_id', WordId, active_bucket_id, <<"unspec">>, frozen_bucket_id, BucketIdList}}} ->
			lager:debug("Word: ~s has no bucket assigned but has frozen buckets: ~p", [Word, BucketIdList]),
			{WordId, BucketIdList};
		
		{ok, {{'_id', WordId, active_bucket_id, BucketId, frozen_bucket_id, []}}} ->
			lager:debug("Word: ~s has bucket assigned: ~p but no fozen buckets.", [Word, BucketId]),
			{WordId, [BucketId]};
		
		{ok, {{'_id', WordId, active_bucket_id, BucketId, frozen_bucket_id, BucketIdList}}} ->
			lager:debug("Word: ~s has buceket: ~p and frozen buckets assinged: ~w", [Word, BucketId, BucketIdList]),
			{WordId, [BucketId | BucketIdList]};
		
		{ok, {}} ->
			lager:debug("There is no entry for word: ~s in db.", [Word]),
			no_word
	end.

%%%
%%% Index retrieving helper functions.
%%%
get_url_id_list(BucketIdList, WordId) ->
	SelectorDoc = {'_id', { bson:utf8("$in"), BucketIdList}}, 
	ProjectionDoc = {indicies, 1, '_id', 0},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(conn_manager_server_master, index),
	{ok, Cursor} = db_helper:perform_action({find, SelectorDoc, ProjectionDoc}, ConnCfg),
	retrieve_url_id_list_from_list_of_db_index_doc_list(WordId, mongo:rest(Cursor), []).
		

retrieve_url_id_list_from_list_of_db_index_doc_list(_, [], ResultUrlIdList) ->
	lager:debug("Url id list collected: ~w", [ResultUrlIdList]),
	ResultUrlIdList;
	
retrieve_url_id_list_from_list_of_db_index_doc_list(WordId, [ {indicies, DbIndexDocList} | T ], ResultUrlIdList) ->
	DbIndexDoc = lists:keyfind(WordId, 2, DbIndexDocList),
	{_, _, _, [UrlIdList, _]} = DbIndexDoc,
	retrieve_url_id_list_from_list_of_db_index_doc_list(WordId, T, UrlIdList ++ ResultUrlIdList).
	