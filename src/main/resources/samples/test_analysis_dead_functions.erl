-module(test_analysis_dead_functions).

-export([start/0, active_entry/1, unused_entry/0]).

%% ====================================================================
%% Entry Points (NOT dead)
%% ====================================================================

%% Exported function
start() ->
    active_private_1().

%% Another export to test multiple exports
active_entry(X) ->
    active_private_2(X).

%% Function that is exported but never called in the module
unused_entry() ->
    ok.


%% ====================================================================
%% Reachable Private Functions (NOT dead)
%% ====================================================================

%% Directly called by start/0
active_private_1() ->
    transitively_active_private().

%% Transitively called by start/0 -> active_private_1/0
transitively_active_private() ->
    ok.

%% Directly called by active_entry/1
active_private_2(X) ->
    X + 1.


%% ====================================================================
%% Unreachable Private Functions (dead)
%% ====================================================================

%% Not called or exported
isolated_dead_private() ->
    unused_atom.

%% Two functions that call eachother, but are not exported and not called elsewhere
dead_clique_a() ->
    dead_clique_b().

dead_clique_b() ->
    dead_clique_a().

%% Recursive private function that calls itself but is not called elsewhere.
recursive_dead_private() ->
    recursive_dead_private().
