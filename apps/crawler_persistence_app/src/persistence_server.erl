-module(persistence_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_index/3, prepare_to_stop/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link([ServerName, HelperServersCfg, PersistenceCfg]) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [HelperServersCfg, PersistenceCfg], []).


add_index(ServerName, Word, UrlId) ->
	gen_server:cast(ServerName, {add_index, Word, UrlId}).


prepare_to_stop(ServerName) ->
	gen_server:call(ServerName, prepare_to_stop).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([HelperServersCfg, PersistenceCfg]) ->
	process_flag(trap_exit, true),
	[RetryTimeCfg] = PersistenceCfg,
	{WordsCacheCfg, IndexCacheCfg, ConnManagerCfg} = HelperServersCfg,
	State = {WordsCacheCfg, IndexCacheCfg, ConnManagerCfg, RetryTimeCfg},
    {ok, State}.


handle_call(prepare_to_stop, _From, State) ->
	lager:debug("Serving request: prepare_to_stop."),
	{reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({add_index, Word, UrlId}, State) ->
	lager:debug("Serving request: {add_index, ~p, ~p}. Mailbox queue size: ~p", 
				[Word, UrlId, element(2,erlang:process_info(self(), message_queue_len)) ]),
	add_index_internal(Word, UrlId, State),
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(shutdown, _State) ->
	ok;

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_word_data(Word, State) ->
	lager:debug("Obtaining id for word: ~p.", [Word]),
	case get_word_id_from_cache(Word, State) of
		word_not_found ->
			{ok, ConnCfg} = conn_manager_server:get_connection_cfg(get_server_name(conn_manager, State), words),
			case get_word_id_from_db(Word, ConnCfg) of
				no_word ->
					{Word, WordId} = create_new_cache_word_doc(Word),
					lager:debug("Word id not found in cache nor in db. New cache word doc created: {~p, ~p}.", 
								[Word, WordId]),
					update_words_cache({add_word_cache_doc, {Word, WordId}}, State),
					wordsdb_functions:save_word(WordId, Word, ConnCfg),
					{WordId, new};
				
				WordId ->
					lager:debug("Word id not found in cache but in db: ~p.", [WordId]),
					update_words_cache({add_word_cache_doc, {Word, WordId}}, State),
					{WordId, was_present}
				
			end;
		
		WordId ->
			lager:debug("Word id found in cache: ~p.", [WordId]),
			{WordId, was_present}
	
	end.


add_index_internal(Word, UrlId, State) ->
	case get_word_data(Word, State) of
		{WordId, was_present} ->
			lager:debug("Updating index: {~p, ~p}", [WordId, UrlId]),
			{ok, ConnCfg} = conn_manager_server:get_connection_cfg(get_server_name(conn_manager, State), index),
			spawn(fun() ->
					indexdb_functions:update_index(WordId, UrlId, ConnCfg)
				end);			
		
		{WordId, new} ->
			lager:debug("Creating new index: {~p, ~p}", [WordId, UrlId]),
			{ok, ConnCfg} = conn_manager_server:get_connection_cfg(get_server_name(conn_manager, State), index),
			indexdb_functions:save_new_index(WordId, UrlId, ConnCfg),
			ok
	end.
			

%%%
%%% Word id obtaining helper functions.
%%%
get_word_id_from_cache(Word, State) ->
	{ok, WordId} = words_cache_server:get_word_id(get_server_name(words, State), Word),
	WordId.

get_word_id_from_db(Word, ConnCfg) ->
	wordsdb_functions:get_word_id(Word, ConnCfg).


%%
%% Words cache handling helper functions.
%%
create_new_cache_word_doc(Word) ->
	{ok, WordId} = id_server:get_word_id(),
	{Word, WordId}.


update_words_cache({add_word_cache_doc, CacheWordDoc}, State) ->
	CacheServerName = get_server_name(words, State),
	case words_cache_server:add_word_cache_doc(CacheServerName, CacheWordDoc) of
		{ok, full} ->
			words_cache_server:flush_and_add_word_cache_doc(CacheServerName, CacheWordDoc),
			lager:debug("Words cache flushed and updated with new cache word doc: ~p.", [CacheWordDoc]);
		
		ok ->
			lager:debug("Words cache updated with new cache word doc: ~p.", [CacheWordDoc]),
			ok
	end.


%%
%% Cache server names helper functions.
%%
get_server_name(words, State) ->
	get_state_value(words_cache_server_name, State);

get_server_name(index, State) ->
	get_state_value(index_cache_server_name, State);

get_server_name(conn_manager, State) ->
	get_state_value(conn_manager_server_name, State).

%%
%% State handling helper functions.
%% 

get_state_value(words_cache_server_name, {{words_cache_server_name, Value}, _, _, _}) ->
	Value;

get_state_value(index_cache_server_name, {_, {index_cache_server_name, Value}, _, _}) ->
	Value;

get_state_value(conn_manager_server_name, {_, _, {conn_manager_server_name, Value}, _}) ->
	Value;

get_state_value(retry_time, {_, _, _, {retry_time, Value}}) ->
	Value.