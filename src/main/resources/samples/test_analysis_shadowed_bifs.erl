-module(test_analysis_shadowed_bifs).

-export([length/1, spawn/1, not_builtin/0]).

% Shadowed functions
length(_List) ->
    0.

spawn(_Fun) ->
    dummy_pid.

% Not shadowed functions
not_builtin() ->
    ok.
