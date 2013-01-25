-module(indexdb_functions).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([save_new_index/3, is_index_present/3, update_index/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
save_new_index(WordId, UrlId, ConnCfg) ->
	DbIndexDoc = {'_id', WordId, urls, [UrlId]},
	db_helper:perform_action({insert, DbIndexDoc}, ConnCfg).
	

is_index_present(WordId, UrlId, ConnCfg) ->
	SelectorDoc = {'_id', WordId, urls, UrlId}, 
	ProjectionDoc = {'_id',0, urls, 0},
	case db_helper:perform_action({find_one, SelectorDoc, ProjectionDoc}, ConnCfg) of
		{ok, {{}}} ->
			%% index is present
				true;
		
		{ok, {}} ->
			false
	end.


update_index(WordId, UrlId, ConnCfg) ->
	SelectorDoc = {'_id', WordId},
	ModifierDoc = { bson:utf8("$push"), {urls, UrlId}},
	db_helper:perform_action({modify, SelectorDoc, ModifierDoc}, ConnCfg).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

	

	
	

