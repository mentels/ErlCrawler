-module(indexdb_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_indicies/3, get_index/2, delete_indicies/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(DbCfg) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, DbCfg, []).

add_indicies(IndiciesList, IndiciesListSize, NewBucketId) ->
	gen_server:cast(?SERVER, {add_indicies, IndiciesList, IndiciesListSize, NewBucketId}).

get_index(BucketId, WordId) ->
	gen_server:call(?SERVER, {get_index, BucketId, WordId}).

delete_indicies(BucketId, WordsIdsList, NewBucketSize) ->
	gen_server:cast(?SERVER, {delete_indicies, BucketId, WordsIdsList, NewBucketSize}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(DbCfg) ->
	process_flag(trap_exit, true),
	[
  		{conn_cfg, ConnCfg},
  		{storage_cfg, StorageCfg},
		_InitialBucketIdCfg
  	] = DbCfg,
	{ok, Conn} = mongo:connect(ConnCfg),
	State = {
			 	{connection, Conn},
				{storage_cfg, StorageCfg}
			 },
    {ok, State}.


handle_call({get_index, BucketId, WordId}, _From, State) ->
	Conn = retrieve_connection(State),
	DbName = retrieve_database_name(State),
	CollName = retrieve_collection_name(State),
	Reply = get_index(BucketId, WordId, DbName, CollName, Conn),
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({add_indicies, IndiciesList, IndiciesListSize, NewBucketId}, State) ->
	Conn = retrieve_connection(State),
	DbName = retrieve_database_name(State),
	CollName = retrieve_collection_name(State),
	add_indicies(IndiciesList, IndiciesListSize, NewBucketId, DbName, CollName, Conn),
	{noreply, State};

handle_cast({delete_indicies, BucketId, WordsIdsList, NewBucketSize}, State) ->
	Conn = retrieve_connection(State),
	DbName = retrieve_database_name(State),
	CollName = retrieve_collection_name(State),
	delete_indicies(BucketId, WordsIdsList, NewBucketSize, DbName, CollName, Conn),
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(shutdown, State) ->
	io:format("indexdb_server terminates...~n"),
	{ {connection, Conn}, _StorageCfg } = State,
	mongo:disconnect(Conn),
	ok;

terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

add_indicies(IndiciesList, IndiciesListSize, NewBucketId, DbName, CollName, Conn) ->
	BucketDoc = {'_id', NewBucketId, bucket_size, IndiciesListSize, indicies, IndiciesList},
	InserAction = create_mongo_action(insert, CollName, BucketDoc, {}),
	perform_mongo_action(InserAction, DbName, Conn).

get_index(BucketId, WordId, DbName, CollName, Conn) ->
	FindDoc = {'_id', BucketId},
	FindAction = create_mongo_action(find, CollName, FindDoc, {}),
	{ok, Cursor} = perform_mongo_action(FindAction, DbName, Conn),
	case mongo:rest(Cursor) of
		[] -> 			{ok, no_bucket};
		[ Doc ] -> 		{IndiciesList} = bson:lookup(indicies, Doc),
						case lists:keyfind(WordId, 2, IndiciesList) of
							false ->
								{ok, no_entry_for_word};
							Index ->
								{ok, Index}
						end;
		[ _H | _T] -> 	{error, multiple_entries}
	end.

delete_indicies(BucketId, WordsIdsList, NewBucketSize,  DbName, CollName, Conn) ->
	SelectorDoc = {'_id', BucketId},
	
	case NewBucketSize of
		0 ->	DeleteBucketAction = create_mongo_action(delete, CollName, SelectorDoc, {}),
				perform_mongo_action(DeleteBucketAction, DbName, Conn);
				
		_ ->	IndiciesModifierDoc = { bson:utf8("$pull"), {indicies, {word_id, { bson:utf8("$in"), WordsIdsList} } } },
				IndiciesModifyAction = create_mongo_action(modify, CollName, IndiciesModifierDoc, SelectorDoc),
				perform_mongo_action(IndiciesModifyAction, DbName, Conn),
	
				BucketSizeModifierDoc = { bson:utf8("$set"), {bucket_size, NewBucketSize} },
				BucketSizeModifyAction = create_mongo_action(modify, CollName, BucketSizeModifierDoc, SelectorDoc),
				perform_mongo_action(BucketSizeModifyAction, DbName, Conn)
	end.

%%
%% Common helper functions.
%%

create_mongo_action(ActionName, Collection, BsonDocument, {}) ->
	case ActionName of
		find	->	Action = fun() ->
						mongo:find(Collection, BsonDocument)
			 		end,
					Action;
		delete	-> Action = fun() ->
						mongo:delete(Collection, BsonDocument)
					end,
				   	Action;
		insert	->  Action = fun() ->
						mongo:insert(Collection, BsonDocument)
					end,
					Action
	end;
create_mongo_action(ActionName, Collection, BsonDocument, SelectorDoc) ->
	case ActionName of
			replace	-> Action = fun() ->
						mongo:replace(Collection, SelectorDoc, BsonDocument)
					end,
				   	Action;
			modify -> Action = fun() ->
						% BsonDocument is modifier in this case
						mongo:modify(Collection, SelectorDoc, BsonDocument)
			end,
			Action
	end.


perform_mongo_action(Action, DbName, Conn) ->
	%Set config.
	WriteMode = safe,
	ReadMode = master,
	
	%Perform action.
	Result =  mongo:do(WriteMode, ReadMode, Conn, DbName, Action),

	%Return result.
	Result.

retrieve_connection(State) ->
	{ {connection, Conn}, _StorageCfg } = State,
	Conn.

retrieve_database_name(State) ->
	{ _ConnCfg, {storage_cfg, [
						_WordsCfg,
						{index, {DbName, _}} 
					   ]}
	} = State,
	DbName.

retrieve_collection_name(State) ->
	{ _ConnCfg, {storage_cfg, [
						_WordsCfg,
						{index, {_, CollectionName}} 
					   ]}
	} = State,
	CollectionName.