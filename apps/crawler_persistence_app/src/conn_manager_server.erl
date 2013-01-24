-module(conn_manager_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, get_connection_cfg/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link([ServerName, ConnManagerCfg]) ->
    gen_server:start_link({local, ServerName}, ?MODULE, ConnManagerCfg, []).


get_connection_cfg(ServerName, CollectionId) when CollectionId == words ->
	gen_server:call(ServerName, {get_conn_cfg_for_words_coll});

get_connection_cfg(ServerName, CollectionId) when CollectionId == index ->
	gen_server:call(?SERVER, {get_conn_cfg_for_index_coll}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(ConnManagerCfg) ->
	process_flag(trap_exit, true),
	[{pool_size, PoolSize}, {host_cfg, HostCfg}, DbNameCfg, WordsCollCfg, IndexCollCfg] = ConnManagerCfg,
	Pool = resource_pool:new(mongo:connect_factory(HostCfg), PoolSize),
	State = {{pool, Pool}, DbNameCfg, WordsCollCfg, IndexCollCfg},  
    {ok, State}.


handle_call({get_conn_cfg_for_words_coll}, _From, State) ->
	{Pool, DbName, WordsCollName} = get_state_value(pool_and_dbname_and_words_collname, State),
	{ok, Conn} = resource_pool:get(Pool),
	{reply, {ok, {WordsCollName, DbName, Conn}}, State};
	
handle_call({get_conn_cfg_for_index_coll}, _From, State) ->
	{Pool, DbName, IndexCollName} = get_state_value(pool_and_dbname_and_index_collname, State),
	{ok, Conn} = resource_pool:get(Pool),
	{reply, {ok, {IndexCollName, DbName, Conn}}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(shutdown, State) ->
  	close_pool(State),
	ok;
  
terminate(_Reason, State) ->
	close_pool(State),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

close_pool(State) ->
	Pool = get_state_value(pool, State),
	resource_pool:close(Pool).

%%
%% State handling helper functions.
%%
get_state_value(pool, {{pool, Pool}, _, _, _}) ->
	Pool;

get_state_value(pool_and_dbname_and_words_collname, {{pool, Pool}, {db_name, DbName}, {words_coll, WordsCollName}, _}) ->
	{Pool, DbName, WordsCollName};

get_state_value(pool_and_dbname_and_index_collname, {{pool, Pool}, {db_name, DbName}, _, {index_coll, IndexCollName}}) ->
	{Pool, DbName, IndexCollName}.
