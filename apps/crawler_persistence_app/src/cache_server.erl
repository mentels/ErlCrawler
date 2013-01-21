-module(cache_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_index/1, update_index_data/2, get_index_data/1, retrieve_all_indicies/0]).
-export([get_state/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(CacheCfg) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, CacheCfg, []).

add_index(CacheDoc) ->
	gen_server:call(?SERVER, {add_index, CacheDoc}).

update_index_data(WordId, IndexData) ->
	gen_server:call(?SERVER, {update_index_data, WordId, IndexData}).

get_index_data(WordId) ->
	gen_server:call(?SERVER, {get_index_data, WordId}).

retrieve_all_indicies() ->
	gen_server:call(?SERVER, {retrieve_all_indicies}).

get_state() ->
	gen_server:call(?SERVER, {get_state}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(CacheCfg) ->
	process_flag(trap_exit, true),
	[MaxCacheSizeCfg] = CacheCfg,
	CacheTabId = ets:new(index_cache, [set, {keypos, 1}, private, named_table]),
	State = {{cache_tab_id, CacheTabId}, {size, 0}, MaxCacheSizeCfg},
    {ok, State}.


handle_call({add_index, CacheDoc}, _From, State) ->
	UpdatedState = update_state({add_index, CacheDoc}, State),
	case is_cache_full(UpdatedState) of
		false -> 
			{reply, ok, UpdatedState};
		true ->
			lager:debug("After adding cache doc: ~w the cache sie full.", [CacheDoc]),
			{reply, {ok, full}, UpdatedState}
	end;

handle_call({update_index_data, WordId, IndexData}, _From, State) ->
	UpdatedState = update_state({update_index_data, WordId, IndexData}, State),
	case is_cache_full(UpdatedState) of
		false -> 
			{reply, ok, UpdatedState};
		true ->
			lager:debug("After updating index for word id: ~p the cache sie full.", [WordId]),
			{reply, {ok, full}, UpdatedState}
	end;

handle_call({get_index_data, WordId}, _From, State) ->
	case get_index_data(WordId, State) of
		{} -> 	
			{reply, {ok, index_not_found}, State};
		IndexData ->	
			{reply, {ok, IndexData}, State}
	end;

handle_call({retrieve_all_indicies}, _From, State) ->
	{CacheDocList, UpdatedState} = update_state({retrieve_all_indicies}, State),
	{reply, {ok, CacheDocList}, UpdatedState};

handle_call({get_state}, _From, State) ->
	{reply, State, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


terminate(shutdown, _State) ->
	lager:debug("Cache server terminating for shutdown reason."),
	ok;

terminate(Reason, _State) ->
	lager:debug("Cache server terminating for reason: ~p", [Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


get_index_data(WordId, State) ->
	CacheTabId = get_state_value(cache_tab_id, State),
	case ets:match(CacheTabId, {WordId, '$0', '$1', '_', '_'}) of
		[] ->
			{};

		[[UrlIdList, UrlIdListSize]] ->
			IndexData = {UrlIdList, UrlIdListSize},
			lager:debug("Index data returned: ~w", [IndexData]),
			IndexData

	end.

%%
%% Higher level state handling functions.
%%
update_state({add_index, CacheDoc}, State) ->
	{_, _, UrlIdListSize, _, _} = CacheDoc,
	{CacheTabId, Size} = get_state_value(cache_tab_id_and_size, State),
	ets:insert(CacheTabId, CacheDoc),
	lager:debug("New cache doc created: ~w; new size is: ~p", [CacheDoc, Size + UrlIdListSize]),
	update_state_value({size, Size + UrlIdListSize}, State);

update_state({update_index_data, WordId, IndexData}, State) ->
	{UrlIdList, UrlIdListSize} = IndexData,
	{CacheTabId, Size} = get_state_value(cache_tab_id_and_size, State),
	ets:update_element(CacheTabId, WordId, [{2, UrlIdList}, {3, UrlIdListSize}]),
	lager:debug("Cache doc for word id: ~p; updated with new index data: ~w", [WordId, IndexData]),
	update_state_value({size, Size + UrlIdListSize}, State);

update_state({retrieve_all_indicies}, State) ->
	{CacheTabId, Size} = get_state_value(cache_tab_id_and_size, State),
	CacheDocList = ets:match_object(CacheTabId, '$1'),
	ets:delete_all_objects(CacheTabId),
	lager:debug("All indicies retrieved: ~w.", [CacheDocList, Size]),
	{CacheDocList, update_state_value({size, 0}, State)}.


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
