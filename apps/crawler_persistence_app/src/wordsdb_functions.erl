-module(wordsdb_functions).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([save_word/1, get_word_data/1, update_active_bucket_id/2, freeze_bucket_id/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

save_word(CacheWordDoc) ->
	save_word_internal(CacheWordDoc).


get_word_data(Word) ->
	get_word_data_internal(Word).


update_active_bucket_id(WordIdList, BucketId) ->
	update_active_bucket_id_internal(WordIdList, BucketId).


freeze_bucket_id(WordIdList, BucketId) ->
	freeze_bucket_id_internal(WordIdList, BucketId).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

save_word_internal({Word, WordId, ActiveBucketId}) ->
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(words),
	DbWordDoc = create_db_word_doc(WordId, Word, ActiveBucketId),
	db_helper:perform_action({insert, DbWordDoc}, ConnCfg).


create_db_word_doc(WordId, Word, ActiveBucketId) when ActiveBucketId == unspec ->
	{'_id', WordId, word, bson:utf8(Word), active_bucket_id, ActiveBucketId, frozen_bucket_id, []};

create_db_word_doc(WordId, Word, ActiveBucketId) when is_integer(ActiveBucketId) ->
	{'_id', WordId, word, bson:utf8(Word), active_bucket_id, ActiveBucketId, frozen_bucket_id, []}.


get_word_data_internal(Word) -> 
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
	db_helper:perform_action({modify, SelectorDoc, ModifierDoc}, ConnCfg).
		

freeze_bucket_id_internal(WordIdList, BucketId) ->
	SelectorDoc = {'_id', { bson:utf8("$in"), WordIdList}},
	ModifierDoc = { bson:utf8("$push"), {frozen_bucket_id, BucketId}},
	{ok, ConnCfg} = conn_manager_server:get_connection_cfg(words),
	db_helper:perform_action({modify, SelectorDoc, ModifierDoc}, ConnCfg).
