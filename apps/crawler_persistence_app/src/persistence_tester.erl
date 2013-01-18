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
-export([start/0, stop/1]).
-export([perform_test/1]).

%%
%% API Functions
%%

start() ->
	spawn(?MODULE, perform_test, [1]).

stop(Pid) ->
	Pid ! stop.


%%
%% Local Functions
%%

perform_test(TestNo) ->
	RandomString = get_random_string(),
	RandomUrlId = get_random_url_id(),
	persistence_server:add_index(RandomString, RandomUrlId),
	receive 
		stop ->
			ok
		after 0 ->
			if
				TestNo == 20000000 ->
					io:format("Testing finished!~n");
				true ->
					%% Simulates the time that is needed for processing subsystem to do its job.
					timer:sleep(10),
					case is_test_no_dividible_by_1k(TestNo) of
						true ->
							io:format("Test no: ~w~n Word: ~s~n UrlId ~w~n", [TestNo, RandomString, RandomUrlId]),
							perform_test(TestNo + 1);
						false ->
							perform_test(TestNo + 1)
					end
			end
	end.
	

is_test_no_dividible_by_1k(TestNo) ->
	if
		TestNo rem 1000 == 0 ->
			true;
		true ->
			false
	end.

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
	random:uniform(26) + 96.

%%
%% Urls.
get_random_url_id() ->
	random:uniform(100000) + 1000.