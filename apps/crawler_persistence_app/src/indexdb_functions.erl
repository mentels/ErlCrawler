-module(indexdb_functions).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([save_new_index/3, get_index_data/2, update_index_data/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
save_new_index(WordId, UrlId, ConnCfg) ->
	DbIndexDoc = {'_id', WordId, urls, [UrlId], urls_cnt, 1},
	db_helper:perform_action({insert, DbIndexDoc}, ConnCfg).
	

get_index_data(WordId, ConnCfg) ->
	SelectorDoc = {'_id', WordId}, 
	ProjectionDoc = {'_id',0, urls, 1, urls_cnt, 1},
	case db_helper:perform_action({find_one, SelectorDoc, ProjectionDoc}, ConnCfg) of
		{ok, {{urls, UrlIdList, urls_cnt, UrlIdListSize}}} ->
				{UrlIdList, UrlIdListSize};
		
		{ok, {}} ->
			no_index
	end.

update_index_data(WordId, UrlIdList, ConnCfg) ->
	SelectorDoc = {'_id', WordId},
	ModifierDoc = { bson:utf8("$inc"), {urls_cnt, 1}, bson:utf8("$set"), {urls, UrlIdList}},
	db_helper:perform_action({modify, SelectorDoc, ModifierDoc}, ConnCfg).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
