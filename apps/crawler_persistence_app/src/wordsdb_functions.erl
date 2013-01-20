-module(wordsdb_functions).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([save_word/1, update_active_bucket_id/2, freeze_bucket_id/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

save_word(Word) ->
	save_word_internal(Word).


update_active_bucket_id(WordIdList, BucketId) ->
	update_active_bucket_id_internal(WordIdList, BucketId).


freeze_bucket_id(WordIdList, BucketId) ->
	freeze_bucket_id_internal(WordIdList, BucketId).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

save_word_internal(Word) ->
	%Check if the word is already in database.
	case get_word_data(Word) of 
		no_word ->			
			%Save new document in the database.
			{ok, WordId} = id_server:get_word_id(),
			DbWordDoc = {'_id', WordId, word, bson:utf8(Word), active_bucket_id, bson:utf8("unspec"), frozen_bucket_id, []},
			{ok, ConnCfg} = conn_manager_server:get_connection_cfg(words),
			db_helper:perform_action({insert, DbWordDoc}, ConnCfg),
			lager:debug("New db word doc saved: ~p.", [DbWordDoc]),
			{ok, {WordId, unspec}};
		
		WordData ->
			lager:debug("Db word doc exists. Word data returned: ~p.", [WordData]),
			{ok, WordData}
	end.


get_word_data(Word) -> 
	SelectorDoc = {word, bson:utf8(Word)},
	ProjectionDoc = {active_bucket_id, 1},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(words),
	case db_helper:perform_action({find_one, SelectorDoc, ProjectionDoc}, ConnCfg) of
		{ok, {{'_id', WordId, active_bucket_id, BucketId}}} ->
			case BucketId of
				<<"unspec">> ->
					{WordId, unspec};
				_ ->
					{WordId, BucketId}
			end;
		
		{ok, {}} ->
			no_word
	end.


update_active_bucket_id_internal(WordIdList, BucketId) ->
	SelectorDoc = {'_id', { bson:utf8("$in"), WordIdList}},
	ModifierDoc = { bson:utf8("$set"), {active_bucket_id, BucketId}},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(words),
	db_helper:perform_action({modify, SelectorDoc, ModifierDoc}, ConnCfg),
	lager:debug("Active bucket id: ~p updated for word id: ~w", [BucketId, WordIdList]).
		

freeze_bucket_id_internal(WordIdList, BucketId) ->
	SelectorDoc = {'_id', { bson:utf8("$in"), WordIdList}},
	ModifierDoc = { bson:utf8("$push"), {frozen_bucket_id, BucketId}},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(words),
	db_helper:perform_action({modify, SelectorDoc, ModifierDoc}, ConnCfg),
	lager:debug("Bucket id: ~p frozen for word id: ~w", [BucketId, WordIdList]).
