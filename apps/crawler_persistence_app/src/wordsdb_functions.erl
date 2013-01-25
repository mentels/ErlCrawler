-module(wordsdb_functions).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([save_word/3, get_word_id/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

save_word(WordId, Word, ConnCfg) ->
	DbWordDoc = {'_id', WordId, word, bson:utf8(Word)},
	db_helper:perform_action({insert, DbWordDoc}, ConnCfg).


get_word_id(Word, ConnCfg) ->
	SelectorDoc = {word, bson:utf8(Word)},
	ProjectionDoc = {'_id', 1},
	case db_helper:perform_action({find_one, SelectorDoc, ProjectionDoc}, ConnCfg) of
		{ok, {{'_id', WordId}}} ->
			WordId;
		
		{ok, {}} ->
			no_word
	end.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

