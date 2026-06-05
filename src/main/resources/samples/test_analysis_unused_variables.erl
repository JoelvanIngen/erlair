-module(test_analysis_unused_variables).

-export([start/0]).

start() ->
    W = 5,  % Used variable
    X = 0,  % Another used variable
    Y = 1,  % Unused variable
    _Z = 2,  % Ignored variable
    _ = 3,  % Ignored "variable"
    
    X + W.
