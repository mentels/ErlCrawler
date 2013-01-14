-module(wordsdb_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_word/1, set_words_bucket_id/2, freeze_bucket_id/2]).

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

add_word(Word) ->
	gen_server:call(?SERVER, {add_word, Word}).

set_words_bucket_id(BucketId, WordsIdsList) ->
	gen_server:cast(?SERVER, {set_words_bucket_id, BucketId, WordsIdsList}).

freeze_bucket_id(WordId, BucketId) ->
	gen_server:cast(?SERVER, {freeze_bucket_id, WordId, BucketId}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(DbCfg) ->
	process_flag(trap_exit, true),
	[
  		{conn_cfg, ConnCfg},
  		{storage_cfg, StorageCfg},
		{initial_bucket_id, InitBucketId}
  	] = DbCfg,
	{ok, Conn} = mongo:connect(ConnCfg),
	State = {
			 	{connection, Conn},
				{storage_cfg, StorageCfg},
				{initial_bucket_id, InitBucketId}				
			 },
    {ok, State}.


handle_call({add_word, Word}, _From, State) ->
	Conn = retrieve_connection(State),
	DbName = retrieve_database_name(State),
	CollName = retrieve_collection_name(State),
	InitBucketId = retrieve_initial_bucket_id(State),
	Reply = add_word(Word, DbName, CollName, Conn, InitBucketId),
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({set_words_bucket_id, BucketId, WordsIdsList}, State) ->
	Conn = retrieve_connection(State),
	DbName = retrieve_database_name(State),
	CollName = retrieve_collection_name(State),
	set_words_bucket_id(BucketId, WordsIdsList, DbName, CollName, Conn),
	{noreply, State};

handle_cast({freeze_bucket_id, WordId, BucketId}, State) ->
	Conn = retrieve_connection(State),
	DbName = retrieve_database_name(State),
	CollName = retrieve_collection_name(State),
	freeze_bucket_id(WordId, BucketId, DbName, CollName, Conn),
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(shutdown, State) ->
	io:format("wordsdb_server terminates...~n"),
	{ {connection, Conn}, _StorageCfg, _InitBucketIdCfg } = State,
	mongo:disconnect(Conn),
	ok;

terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%%
%% Database operations' functions.
%%

add_word(Word, DbName, CollName, Conn, InitBucketId) ->
	
	%Check if the word is already in database.
	case get_word_id(Word, DbName, CollName, Conn) of 
		{ok, no_entry_for_word} ->			%Save new document in the database.
											WordId = id_server:get_id(),
											NewWordDoc = {'_id', WordId, word, bson:utf8(Word), bucket_id, InitBucketId},
											InsertAction = create_mongo_action(insert, CollName, NewWordDoc, {}),
											perform_mongo_action(InsertAction, DbName, Conn),
											{ok, {WordId, InitBucketId}};
		
		{ok, WordData} ->					{ok, WordData};
		{error, multiple_entries_for_word} -> {error, multiple_entries_for_word}
	end.

get_word_id(Word, DbName, CollName, Conn) -> 
	
	SearchWordDoc = {word, bson:utf8(Word)},
	FindAction = create_mongo_action(find, CollName, SearchWordDoc, {}),
	{ok, Cursor} = perform_mongo_action(FindAction, DbName, Conn),
	case mongo:rest(Cursor) of
		[] ->	 			{ok, no_entry_for_word};
		[ Doc ] ->   		{WordId} = bson:lookup('_id', Doc),
							{BucketId} = bson:lookup('bucket_id', Doc),
							{ok, {WordId, BucketId}};
		[ _H | _T ] ->		{error, multiple_entries_for_word}
	end.

set_words_bucket_id(BucketId, WordsIdsList, DbName, CollName, Conn) ->
	SelectorDoc = {'_id', { bson:utf8("$in"), WordsIdsList}},
	ModifierDoc = { bson:utf8("$set"), {bucket_id, BucketId}},
	ModifyAction = create_mongo_action(modify, CollName, ModifierDoc, SelectorDoc),
	perform_mongo_action(ModifyAction, DbName, Conn).

freeze_bucket_id(WordId, BucketId, DbName, CollName, Conn) ->
	SelectorDoc = {'_id', WordId},
	ModifierDoc = { bson:utf8("$push"), {frozen_bucket_id, BucketId}},
	ModifyAction = create_mongo_action(modify, CollName, ModifierDoc, SelectorDoc),
	perform_mongo_action(ModifyAction, DbName, Conn).

%%
%% Common helper functions.
%%

create_mongo_action(ActionName, Collection, BsonDocument, {}) ->
	case ActionName of
		find	->	Action = fun() ->
						mongo:find(Collection, BsonDocument)
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
	{ {connection, Conn}, _, _ } = State,
	Conn.

retrieve_database_name(State) ->
	{ _, {storage_cfg, [
						{words, {DbName, _}}, 
						_
					   ]}, _ } = State,
	DbName.

retrieve_collection_name(State) ->
	{ _, {storage_cfg, [
						{words, {_, CollectionName}}, 
						_
					   ]}, _ } = State,
	CollectionName.

retrieve_initial_bucket_id(State) ->
	{ _, _, {initial_bucket_id, InitBucketId} } = State,
	InitBucketId.