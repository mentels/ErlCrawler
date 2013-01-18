-module(cache_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_index/1	, retrieve_index/1, retrieve_old_indicies/0, retrieve_all_indicies/0]).
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

retrieve_index(WordId) ->
	gen_server:call(?SERVER, {retrieve_index, WordId}).

retrieve_old_indicies() ->
	gen_server:call(?SERVER, {retrieve_old_indicies}).

retrieve_all_indicies() ->
	gen_server:call(?SERVER, {retrieve_all_indicies}).

get_state() ->
	gen_server:call(?SERVER, {get_state}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(CacheCfg) ->
	process_flag(trap_exit, true),
	[PercentageToFlushCfg, MaxCacheSizeCfg] = CacheCfg,
	State = {{cache, []}, {size, 0}, PercentageToFlushCfg, MaxCacheSizeCfg},
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

handle_call({retrieve_index, WordId}, _From, State) ->
	{CacheDoc, UpdatedState} = update_state({retrieve_index, WordId}, State),
	case CacheDoc of
		{} -> 	
			{reply, {ok, index_not_found}, UpdatedState};
		_ ->	
			{reply, {ok, CacheDoc}, UpdatedState}
	end;

handle_call({retrieve_old_indicies}, _From, State) ->
	%% We count how much urls ids we want to retrieve.
	RemainingUrlIdCount = calculate_remaining_url_id_count(State),
	lager:debug("Calculated count of remaning url ids: ~p", [RemainingUrlIdCount]),
	{CacheDocList, UpdatedState} = update_state({retrieve_old_indicies, RemainingUrlIdCount}, State),
	{reply, {ok, CacheDocList}, UpdatedState};

handle_call({retrieve_all_indicies}, _From, State) ->
	Cache = get_state_value(cache, State),
	{reply, {ok, Cache}, State};

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


%%
%% Higher level state handling functions.
%%
update_state({add_index, CacheDoc}, State) ->
	{_, _, UrlIdListSize, _, _} = CacheDoc,
	{Cache, Size} = get_state_value(cache_and_size, State),
	lager:debug("New cache doc created: ~w; new size is: ~p", [CacheDoc, Size + UrlIdListSize]),
	update_state_value({cache_and_size, [ CacheDoc | Cache ], Size + UrlIdListSize}, State);

update_state({retrieve_index, WordId}, State) ->
	{Cache, Size} = get_state_value(cache_and_size, State),
	{CacheDoc, UpdatedCache} = update_cache({retrieve_index, WordId, []}, Cache),
	case CacheDoc of
		{_, _, UrlIdListSize, _, _} ->
			lager:debug("Cache doc retrieved: ~w; new size is: ~p", [CacheDoc, Size - UrlIdListSize]),
			{CacheDoc, update_state_value({cache_and_size, UpdatedCache, Size - UrlIdListSize}, State)};
		
		{} ->
			{{}, State}
	end;

update_state({retrieve_old_indicies, RemainingUrlIdCount}, State) ->
	Cache = get_state_value(cache, State),
	{CacheDocList, UpdatedCache, Size} = update_cache({retrieve_old_indicies, RemainingUrlIdCount, 0, []}, Cache),
	lager:debug("Old indicies retrieved: ~w; new size is: ~p", [CacheDocList, Size]),
	{CacheDocList, update_state_value({cache_and_size, UpdatedCache, Size}, State)}.

	
%%
%% Cache handling helper functions.
%%
update_cache({retrieve_index, _, ProcessedCache}, []) ->
	{{}, ProcessedCache};

update_cache({retrieve_index, WordId, ProcessedCache}, [ CacheDoc | Cache ]) ->
	case is_cache_doc_for_word_id(WordId, CacheDoc) of 
		true ->		
			{CacheDoc, ProcessedCache ++ Cache};
		false -> 	
			update_cache({retrieve_index, WordId, ProcessedCache ++ [CacheDoc]}, Cache)
	end;

update_cache({retrieve_old_indicies, RemainingUrlIdCount, ProccessedUrlIdCnt, ProcessedCache}, [CacheDoc | Cache]) ->
	{_, _, UrlIdListSize, _, _} = CacheDoc,
	if
		UrlIdListSize + ProccessedUrlIdCnt > RemainingUrlIdCount ->
			{[CacheDoc] ++ Cache, ProcessedCache, ProccessedUrlIdCnt};
		true ->
			update_cache({retrieve_old_indicies, RemainingUrlIdCount, ProccessedUrlIdCnt + UrlIdListSize, ProcessedCache ++ [CacheDoc]}, Cache)
	end.


%%
%% Calculation helper functions.
%%
calculate_remaining_url_id_count(State) ->
	Size = get_state_value(size, State),
	PercentageToFlush = get_state_value(percentage_to_flush, State),
	Size - (Size * PercentageToFlush) div 100.	


is_cache_full(State) ->
	Size = get_state_value(size, State),
	MaxSize = get_state_value(max_size, State),
	if
		Size >= MaxSize ->
			true;
		true ->
			false
	end.


is_cache_doc_for_word_id(WordId, {ExaminedWordId, _, _, _, _}) ->
	if
		WordId == ExaminedWordId ->
			true;
		true ->
			false
	end.


%%
%% State handling helper functions.
%%
get_state_value(cache_and_size, {{cache, Cache}, {size, Size}, _, _}) ->
	{Cache, Size};

get_state_value(cache, {{cache, Cache}, _, _, _}) ->
	Cache;

get_state_value(size, {_, {size, Size}, _, _}) ->
	Size;

get_state_value(max_size, {_, _, _, {max_cache_size, Value}}) ->
	Value;

get_state_value(percentage_to_flush, {_, _, {percentage_to_flush, Value}, _}) ->
	Value.


update_state_value({cache_and_size, Cache, Size}, {_, _, Cfg1, Cfg2}) ->
	{{cache, Cache}, {size, Size}, Cfg1, Cfg2};
	
update_state_value({cache, Cache}, {_, SizeCfg, Cfg1, Cfg2}) ->
	{{cache, Cache}, SizeCfg, Cfg1, Cfg2};

update_state_value({size, Size}, {CacheCfg, _, Cfg1, Cfg2}) ->
	{{cache, CacheCfg}, {size, Size}, Cfg1, Cfg2}. 
