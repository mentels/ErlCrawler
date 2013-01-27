-module(indexdb_functions).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([save_new_index/3, update_index/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
save_new_index(WordId, UrlId, ConnCfg) ->
	DbIndexDoc = {'_id', WordId, urls, [UrlId]},
	db_helper:perform_action({insert, DbIndexDoc}, ConnCfg).
	

update_index(WordId, UrlId, ConnCfg) ->
	SelectorDoc = {'_id', WordId},
	ModifierDoc = { bson:utf8("$addToSet"), {urls, UrlId}},
	db_helper:perform_action({modify, SelectorDoc, ModifierDoc}, ConnCfg).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
