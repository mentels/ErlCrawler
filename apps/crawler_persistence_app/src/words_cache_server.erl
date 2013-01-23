-module(words_cache_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_word_cache_doc/2, flush_and_add_word_cache_doc/2, get_word_data/2, flush/1]).
-export([get_state/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link([ServerName, WordsCacheCfg]) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [ServerName, WordsCacheCfg], []).


add_word_cache_doc(ServerName, CacheWordDoc) ->
	gen_server:call(ServerName, {add_word_cache_doc, CacheWordDoc}).


flush_and_add_word_cache_doc(ServerName, CacheWordDoc) ->
	gen_server:cast(ServerName, {flush_and_add_word_cache_doc, CacheWordDoc}).


get_word_data(ServerName, Word) ->
	gen_server:call(ServerName, {get_word_data, Word}).


flush(ServerName) ->
	gen_server:cast(ServerName, flush).


get_state(ServerName) ->
	gen_server:call(ServerName, get_state).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ServerName, WordsCacheCfg]) ->
	process_flag(trap_exit, true),
	[MaxCacheSizeCfg] = WordsCacheCfg,
	EtsName = list_to_atom(atom_to_list(ServerName) ++ "_cache"),
	CacheTabId = ets:new(EtsName, [set, {keypos, 1}, private, named_table]),
	State = {{cache_tab_id, CacheTabId}, {size, 0}, MaxCacheSizeCfg},
    {ok, State}.


handle_call({add_word_cache_doc, CacheWordDoc}, _From, State) ->
	case is_cache_full(State) of
		false -> 
			UpdatedState = update_state({add_word_cache_doc, CacheWordDoc}, State),
			{reply, ok, UpdatedState};
		true ->
			{reply, {ok, full}, State}
	end;

handle_call({get_word_data, Word}, _From, State) ->
	case get_word_data_internal(Word, State) of
		{} -> 	
			{reply, {ok, word_not_found}, State};
		WordData ->	
			{reply, {ok, WordData}, State}
	end;

handle_call(get_state, _From, State) ->
	{reply, State, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.


handle_cast({flush_and_add_word_cache_doc, CacheWordDoc}, State) ->
	UpdatedState = update_state(flush, State),
	{noreply, update_state({add_word_cache_doc, CacheWordDoc}, UpdatedState)};

handle_cast(flush, State) ->
	UpdatedState = update_state(flush, State),
	{noreply, UpdatedState};

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


get_word_data_internal(Word, State) ->
	CacheTabId = get_state_value(cache_tab_id, State),
	case ets:match(CacheTabId, {Word, '$0', '$1'}) of
		[] ->
			{};

		[[WordId, ActiveBucketId]] ->
			WordData = {WordId, ActiveBucketId},
			WordData

	end.

%%
%% Higher level state handling functions.
%%
update_state({add_word_cache_doc, CacheWordDoc}, State) ->
	{CacheTabId, Size} = get_state_value(cache_tab_id_and_size, State),
	ets:insert(CacheTabId, CacheWordDoc),
	update_state_value({size, Size + 1}, State);

update_state(flush, State) ->
	CacheTabId = get_state_value(cache_tab_id, State),
	ets:delete_all_objects(CacheTabId),
	update_state_value({size, 0}, State).


%%
%% Calculation helper functions.
%%
is_cache_full(State) ->
	Size = get_state_value(size, State),
	MaxSize = get_state_value(max_size, State),
	if
		Size >= MaxSize ->
			true;
		true ->
			false
	end.


%%
%% State handling helper functions.
%%
get_state_value(cache_tab_id_and_size, {{cache_tab_id, CacheTabId}, {size, Size}, _}) ->
	{CacheTabId, Size};

get_state_value(cache_tab_id, {{cache_tab_id, Cache}, _, _}) ->
	Cache;

get_state_value(size, {_, {size, Size}, _}) ->
	Size;

get_state_value(max_size, {_, _, {max_cache_size, Value}}) ->
	Value.


update_state_value({size, Size}, {CacheCfg, _, MaxCacheSizeCfg}) ->
	{CacheCfg, {size, Size}, MaxCacheSizeCfg}. 
