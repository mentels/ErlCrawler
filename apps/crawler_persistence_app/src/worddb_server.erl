-module(worddb_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, save_word/1, update_active_bucket_id/2, freeze_bucket_id/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(WordDbCfg) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, WordDbCfg, []).

save_word(Word) ->
	gen_server:call(?SERVER, {save_word, Word}).

update_active_bucket_id(WordIdList, BucketId) ->
	gen_server:cast(?SERVER, {update_active_bucket_id, WordIdList, BucketId}).

freeze_bucket_id(WordIdList, BucketId) ->
	gen_server:cast(?SERVER, {freeze_bucket_id, WordIdList, BucketId}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(WordDbCfg) ->
	[
  		{conn_cfg, ConnCfg},
  		{db, DbName},
		{coll, CollName}
  	] = WordDbCfg,
	{ok, Conn} = mongo:connect(ConnCfg),
	State = {
				{coll, CollName},
				{db, DbName},
				{conn, Conn}
			 },
    {ok, State}.


handle_call({save_word, Word}, _From, State) ->
	Reply = save_word(Word, State),
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({update_active_bucket_id, WordIdList, BucketId}, State) ->
	update_active_bucket_id(WordIdList, BucketId, State),
	{noreply, State};

handle_cast({freeze_bucket_id, WordIdList, BucketId}, State) ->
	freeze_bucket_id(WordIdList, BucketId, State),
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

save_word(Word, State) ->
	%Check if the word is already in database.
	case get_word_data(Word, State) of 
		no_word ->			
			%Save new document in the database.
			{ok, WordId} = id_server:get_word_id(),
			DbWordDoc = {'_id', WordId, word, bson:utf8(Word), active_bucket_id, bson:utf8("unspec"), frozen_bucket_id, []},
			{CollName, DbName, Conn} = retrieve_collname_dbname_conn(State),
			db_helper:perform_action({insert, DbWordDoc}, CollName, DbName, Conn),
			lager:debug("New db word doc saved: ~p.", [DbWordDoc]),
			{ok, {WordId, unspec}};
		
		WordData ->
			lager:debug("Db word doc exists. Word data returned: ~p.", [WordData]),
			{ok, WordData}
	end.


get_word_data(Word, State) -> 
	SelectorDoc = {word, bson:utf8(Word)},
	ProjectionDoc = {active_bucket_id, 1},
	{CollName, DbName, Conn} = retrieve_collname_dbname_conn(State),
	case db_helper:perform_action({find_one, SelectorDoc, ProjectionDoc}, CollName, DbName, Conn) of
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


update_active_bucket_id(WordIdList, BucketId, State) ->
	SelectorDoc = {'_id', { bson:utf8("$in"), WordIdList}},
	ModifierDoc = { bson:utf8("$set"), {active_bucket_id, BucketId}},
	{CollName, DbName, Conn} = retrieve_collname_dbname_conn(State),
	db_helper:perform_action({modify, SelectorDoc, ModifierDoc}, CollName, DbName, Conn),
	lager:debug("Active bucket id: ~p updated for word id: ~w", [BucketId, WordIdList]).
		

freeze_bucket_id(WordIdList, BucketId, State) ->
	SelectorDoc = {'_id', { bson:utf8("$in"), WordIdList}},
	ModifierDoc = { bson:utf8("$push"), {frozen_bucket_id, BucketId}},
	{CollName, DbName, Conn} = retrieve_collname_dbname_conn(State),
	db_helper:perform_action({modify, SelectorDoc, ModifierDoc}, CollName, DbName, Conn),
	lager:debug("Bucket id: ~p frozen for word id: ~w", [BucketId, WordIdList]).


%%
%% State handling helper functions.
%%
retrieve_collname_dbname_conn(State) ->
	{{coll, CollName}, {db, DbName}, {conn, Conn}} = State,
	{CollName, DbName, Conn}.	