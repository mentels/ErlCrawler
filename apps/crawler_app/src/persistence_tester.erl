%% Author: jason
%% Created: Oct 29, 2012
%% Description: TODO: Add description to persistence_tester
-module(persistence_tester).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start_testing/0]).
-export([perform_test/1]).

%%
%% API Functions
%%

start_testing() ->
	spawn(?MODULE, perform_test, [1]).


%%
%% Local Functions
%%

%%
%% Counts files' lines.
%%

perform_test(TestNo) ->
	RandomString = get_random_string(),
	RandomUrlId = get_random_url_id(),
	persistence_server:add_index(RandomString, RandomUrlId),
	io:format("Test no: ~w~n Word: ~w~n UrlId ~w~n", [TestNo, RandomString, RandomUrlId]),
	perform_test(TestNo + 1).

%%%
%%% Generating random stuff. 
%%%

%%
%% Strings.
get_random_string() ->
	get_random_string(random:uniform(15)).

get_random_string(0) ->
	[];
get_random_string(Length) ->
	[get_random_char() | get_random_string(Length - 1)].

get_random_char() ->
	random:uniform(122) + 97.

%%
%% Urls.
get_random_url_id() ->
	random:uniform(100000) + 1000.