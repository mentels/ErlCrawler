-module(id_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, get_word_id/1, get_bucket_id/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link([ServerName, ChannelsCfg, IdCfg]) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [ChannelsCfg, IdCfg], []).

get_word_id() ->
	gen_server:call(?SERVER, get_word_id).

get_bucket_id() ->
	gen_server:call(?SERVER, get_bucket_id).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ChannelsCfg, IdCfg]) ->
	[   
	 	{init_word_id, WordId},
		{init_bucket_id, BucketId}
	] = IdCfg,
	{{channel_id, ChannelId}, {channels_cnt, ChannelsCnt}} = ChannelsCfg,
	%% The state always holds the ids that will be returned in the next calls to 
	%% either get_word_id/0 or get_bucket_id/0.
	State = {WordId + ChannelId, BucketId + ChannelId, ChannelsCnt},
    {ok, State}.

handle_call(get_word_id, _From, {WordId, _BucketId, ChannelsCnt}) ->
	{reply, {ok, WordId}, {WordId + ChannelsCnt, _BucketId, ChannelsCnt}};

handle_call(get_bucket_id, _From, {_WordId, BucketId, ChannelsCnt}) ->
	{reply, {ok, BucketId}, {_WordId, BucketId + ChannelsCnt, ChannelsCnt}};

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

