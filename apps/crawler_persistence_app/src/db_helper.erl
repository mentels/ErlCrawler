-module(db_helper).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([perform_action/2, perform_spawned_action/2]).

%% ------------------------------------------------------------------
%% Internal Function Exports
%% ------------------------------------------------------------------

-export([perform_db_action/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

perform_action(ActionDesc, ConnCfg) ->
	perform_db_action(ActionDesc, ConnCfg).	

perform_spawned_action(ActionDesc, ConnCfg) ->
	spawn(?MODULE, perform_db_action, [ActionDesc, ConnCfg]).	

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

perform_db_action({find, Doc, ProjectionDoc}, {CollName, DbName, Conn}) ->
	Action = fun() -> mongo:find(CollName, Doc, ProjectionDoc) end,
	perform_mongo_action(Action, DbName, Conn);

perform_db_action({find_one, Doc, ProjectionDoc}, {CollName, DbName, Conn}) ->
	Action = fun() -> mongo:find_one(CollName, Doc, ProjectionDoc) end,
	perform_mongo_action(Action, DbName, Conn);

perform_db_action({delete, Doc}, {CollName, DbName, Conn}) ->
	Action = fun() -> mongo:delete(CollName, Doc) end,
	perform_mongo_action(Action, DbName, Conn);

perform_db_action({insert, Doc}, {CollName, DbName, Conn}) ->
	Action = fun() -> mongo:insert(CollName, Doc) end,
	perform_mongo_action(Action, DbName, Conn);

perform_db_action({replace, SelectorDoc, Doc}, {CollName, DbName, Conn}) -> 
	Action = fun() -> mongo:replace(CollName, SelectorDoc, Doc) end,
	perform_mongo_action(Action, DbName, Conn);

perform_db_action({modify, SelectorDoc, ModifierDoc}, {CollName, DbName, Conn}) ->
	Action = fun() -> mongo:modify(CollName, SelectorDoc, ModifierDoc) end,
	perform_mongo_action(Action, DbName, Conn);

perform_db_action({create_index, IndexSpec}, {CollName, DbName, Conn}) ->
	Action = fun() -> mongo:create_index(CollName, IndexSpec) end,
	perform_mongo_action(Action, DbName, Conn).


perform_mongo_action(Action, DbName, Conn) ->
	WriteMode = unsafe,
	ReadMode = master,
	case mongo:do(WriteMode, ReadMode, Conn, DbName, Action) of
		{failure, FailureDesc} ->
			lager:error("Mongo action failure: ~p", [FailureDesc]),
			{failure, FailureDesc};
		Result ->
			Result
	end.
			
