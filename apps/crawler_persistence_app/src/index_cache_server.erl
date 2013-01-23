-module(index_cache_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_index/2, update_index_data/3, get_index_data/2, retrieve_all_indicies/1]).
-export([get_state/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link([ServerName, CacheCfg]) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [ServerName, CacheCfg], []).

add_index(ServerName, CacheDoc) ->
	gen_server:call(ServerName, {add_index, CacheDoc}).

update_index_data(ServerName, WordId, IndexData) ->
	gen_server:call(ServerName, {update_index_data, WordId, IndexData}).

get_index_data(ServerName, WordId) ->
	gen_server:call(ServerName, {get_index_data, WordId}).

retrieve_all_indicies(ServerName) ->
	gen_server:call(ServerName, {retrieve_all_indicies}).

get_state(ServerName) ->
	gen_server:call(ServerName, {get_state}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ServerName, CacheCfg]) ->
	process_flag(trap_exit, true),
	[MaxCacheSizeCfg] = CacheCfg,
	EtsName = list_to_atom(atom_to_list(ServerName) ++ "_cache"),
	CacheTabId = ets:new(EtsName, [set, {keypos, 1}, private, named_table]),
	State = {{cache_tab_id, CacheTabId}, {size, 0}, MaxCacheSizeCfg},
    {ok, State}.


handle_call({add_index, CacheDoc}, _From, State) ->
	UpdatedState = update_state({add_index, CacheDoc}, State),
	case is_cache_full(UpdatedState) of
		false -> 
			{reply, ok, UpdatedState};
		true ->
			{reply, {ok, full}, UpdatedState}
	end;

handle_call({update_index_data, WordId, IndexData}, _From, State) ->
	UpdatedState = update_state({update_index_data, WordId, IndexData}, State),
	case is_cache_full(UpdatedState) of
		false -> 
			{reply, ok, UpdatedState};
		true ->
			{reply, {ok, full}, UpdatedState}
	end;

handle_call({get_index_data, WordId}, _From, State) ->
	case get_index_data_internal(WordId, State) of
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
	ok;

terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


get_index_data_internal(WordId, State) ->
	CacheTabId = get_state_value(cache_tab_id, State),
	case ets:match(CacheTabId, {WordId, '$0', '$1', '_', '_'}) of
		[] ->
			{};

		[[UrlIdList, UrlIdListSize]] ->
			IndexData = {UrlIdList, UrlIdListSize},
			IndexData

	end.

%%
%% Higher level state handling functions.
%%
update_state({add_index, CacheDoc}, State) ->
	{_, _, UrlIdListSize, _, _} = CacheDoc,
	{CacheTabId, Size} = get_state_value(cache_tab_id_and_size, State),
	ets:insert(CacheTabId, CacheDoc),
	update_state_value({size, Size + UrlIdListSize}, State);

update_state({update_index_data, WordId, IndexData}, State) ->
	{UrlIdList, UrlIdListSize} = IndexData,
	{CacheTabId, Size} = get_state_value(cache_tab_id_and_size, State),
	ets:update_element(CacheTabId, WordId, [{2, UrlIdList}, {3, UrlIdListSize}]),
	update_state_value({size, Size + 1}, State);

update_state({retrieve_all_indicies}, State) ->
	CacheTabId = get_state_value(cache_tab_id, State),
	CacheDocList = ets:match_object(CacheTabId, '$1'),
	ets:delete_all_objects(CacheTabId),
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
