-module(indexdb_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, save_indicies/4, get_index/2, get_url_cnt/1, delete_indicies/4, delete_bucket/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(IndexDbCfg) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, IndexDbCfg, []).

save_indicies(BucketId, WordCnt, UrlCnt, IncompleCacheDocList) ->
	gen_server:cast(?SERVER, {save_indicies, BucketId, WordCnt, UrlCnt, IncompleCacheDocList}).

get_index(BucketId, WordId) ->
	gen_server:call(?SERVER, {get_index, BucketId, WordId}).

get_url_cnt(BucketId) ->
	gen_server:call(?SERVER, {get_url_cnt, BucketId}).

delete_indicies(BucketId, WordIdList, WordCntDiff, NewUrlCnt) ->
	gen_server:cast(?SERVER, {delete_indicies, BucketId, WordIdList, WordCntDiff, NewUrlCnt}).

delete_bucket(BucketId) ->
	gen_server:cast(?SERVER, {delete_bucket, BucketId}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(IndexDbCfg) ->
	process_flag(trap_exit, true),
	[
  		{conn_cfg, ConnCfg},
  		{db, DbName},
		{coll, CollName}
  	] = IndexDbCfg,
	{ok, Conn} = mongo:connect(ConnCfg),
	State = {
				{coll, CollName},
				{db, DbName},
				{conn, Conn}
			 },
    {ok, State}.


handle_call({get_index, BucketId, WordId}, _From, State) ->
	Reply = get_index(BucketId, WordId, State),
	{reply, Reply, State};

handle_call({get_url_cnt, BucketId}, _From, State) ->
	Reply = get_url_cnt(BucketId, State),
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({save_indicies, BucketId, WordCnt, UrlCnt, IncompleCacheDocList}, State) ->
	save_indicies(BucketId, WordCnt, UrlCnt, IncompleCacheDocList, State),
	{noreply, State};

handle_cast({delete_indicies, BucketId, WordIdList, WordCntDiff, NewUrlCnt}, State) ->
	delete_indicies(BucketId, WordIdList, WordCntDiff, NewUrlCnt, State),
	{noreply, State};

handle_cast({delete_bucket, BucketId}, State) ->
	delete_bucket(BucketId, State),
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(shutdown, State) ->
	lager:debug("Indexdb server terminating for shutdown reason."),
	{_, _, Conn} = retrieve_collname_dbname_conn(State),
	mongo:disconnect(Conn),
	ok;

terminate(Reason, State) ->
	lager:debug("Indexdb server terminating for reason: ~p", [Reason]),
	{_, _, Conn} = retrieve_collname_dbname_conn(State),
	mongo:disconnect(Conn),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

save_indicies(BucketId, WordCnt, UrlCnt, IncompleteCacheDocList, State) ->
	DbIndexDocList = convert_incomplete_cache_docs_to_db_index_docs(IncompleteCacheDocList, []),
	DbBucketDoc = {'_id', BucketId, word_cnt, WordCnt, url_cnt, UrlCnt, indicies, DbIndexDocList},
	{CollName, DbName, Conn} = retrieve_collname_dbname_conn(State),
	db_helper:perform_action({insert, DbBucketDoc}, CollName, DbName, Conn),
	lager:debug("Db bucket doc saved: ~p", [DbBucketDoc]).


get_index(BucketId, WordId, State) ->
	SelectorDoc = {'_id', BucketId}, 
	ProjectionDoc = {indicies, 1, '_id', 0},
	{CollName, DbName, Conn} = retrieve_collname_dbname_conn(State),
	case db_helper:perform_action({find_one, SelectorDoc, ProjectionDoc}, CollName, DbName, Conn) of
		{ok, {{indicies, DbIndexDocList}}} ->
			case lists:keyfind(WordId, 2, DbIndexDocList) of
				false -> 
					lager:debug("No word id: ~p in bucket id: ~p.", [WordId, BucketId]),
					{ok, no_word};
				DbIndexDoc ->
					IncompleteCacheDoc = convert_db_doc_to_incomplete_cache_doc(DbIndexDoc),
					lager:debug("Incomplete cache doc returned: ~p", [IncompleteCacheDoc]),
					{ok, IncompleteCacheDoc}
			end;

		{ok, {}} ->
			lager:debug("No bucket id: ~p", [BucketId]),
			{ok, no_bucket}
	end.
	

get_url_cnt(BucketId, State) ->
	SelectorDoc = {'_id', BucketId}, 
	ProjectionDoc = {url_cnt, 1, '_id', 0},
	{CollName, DbName, Conn} = retrieve_collname_dbname_conn(State),
	case db_helper:perform_action({find_one, SelectorDoc, ProjectionDoc}, CollName, DbName, Conn) of
		{ok, {{url_cnt, UrlIdCnt}}} ->
			lager:debug("Url cnt: ~p returned for bucket id: ~p", [UrlIdCnt, BucketId]),
			{ok, UrlIdCnt};

		{ok, {}} ->
			lager:debug("No bucket id: ~p", [BucketId]),
			{ok, no_bucket}
	end.

	
delete_indicies(BucketId, WordIdList, WordCntDiff, NewUrlCnt, State) ->
	SelectorDoc = {'_id', BucketId},
	{CollName, DbName, Conn} = retrieve_collname_dbname_conn(State),
	
	%% Delete from given bucket those entries that are related to word's ids 
	%% contained on the WordIdList. 
	IndiciesModifierDoc = { bson:utf8("$pull"), {indicies, {word_id, { bson:utf8("$in"), WordIdList}}}},
	db_helper:perform_action({modify, SelectorDoc, IndiciesModifierDoc}, CollName, DbName, Conn),
	lager:debug("From bucket id: ~w db index docs deleted: ~w", [BucketId, WordIdList]),
	
	%% Update words' ids counter.
	WordCntModifierDoc = { bson:utf8("$inc"), {word_cnt, WordCntDiff} },
	db_helper:perform_action({modify, SelectorDoc, WordCntModifierDoc}, CollName, DbName, Conn),
	lager:debug("Bucket id: ~w updated with new word diff: ~p", [BucketId, WordCntDiff]),

	%% Update urls' ids counter.
	UrlCntModifierDoc = { bson:utf8("$set"), {url_cnt, NewUrlCnt} },
	db_helper:perform_action({modify, SelectorDoc, UrlCntModifierDoc}, CollName, DbName, Conn),
	lager:debug("Bucket id: ~w updated with new url cnt: ~p", [BucketId, NewUrlCnt]).


delete_bucket(BucketId, State) ->
	SelectorDoc = {'_id', BucketId},
	{CollName, DbName, Conn} = retrieve_collname_dbname_conn(State),
	db_helper:perform_action({delete, SelectorDoc}, CollName, DbName, Conn),
	lager:debug("Bucket id: ~p deleted from db.", [BucketId]).

%%
%% Data handling herlper functions.
%%
convert_incomplete_cache_docs_to_db_index_docs([], DbIndexDocList) ->
	DbIndexDocList;
convert_incomplete_cache_docs_to_db_index_docs([ IncompleteCacheDoc | IncompleteCacheDocList], DbIndexDocList) ->
	{WordId, UrlIdList, UrlIdListSize} = IncompleteCacheDoc,
	DbIndexDoc = {word_id, WordId, data, [UrlIdList, UrlIdListSize]},
	convert_incomplete_cache_docs_to_db_index_docs(IncompleteCacheDocList, DbIndexDocList ++ [DbIndexDoc]).


convert_db_doc_to_incomplete_cache_doc(DbIndexDoc) ->
	{word_id, WordId, data, [UrlIdList, UrlIdListSize]} = DbIndexDoc,
	{WordId, UrlIdList, UrlIdListSize}.


%%
%% State handling helper functions.
%%
retrieve_collname_dbname_conn(State) ->
	{{coll, CollName}, {db, DbName}, {conn, Conn}} = State,
	{CollName, DbName, Conn}.		
