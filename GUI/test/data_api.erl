%% @author maciek
%% @doc @todo Add description to data_api.


-module(data_api).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_index/1,get_count/1]).

get_index(_) ->
	{A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
	[[random:uniform(100),random:uniform(100),random:uniform(100)]].

get_count(indexes) ->
	{A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
	{ok, random:uniform(100000)};
	
get_count(words_coll) ->
	{A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
	{ok, random:uniform(100000)};
								   
get_count(index_coll) ->
	{A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
	{ok, random:uniform(100000)}.
								   

