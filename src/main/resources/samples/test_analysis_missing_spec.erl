-module(test_analysis_missing_spec).

-export([public_with_spec/1, public_no_spec/1]).

% Public functions
% Has spec, should not be in report
-spec public_with_spec(integer()) -> integer().
public_with_spec(X) -> 
    X + 1.

% Has no spec, should be in report
public_no_spec(X) -> 
    private_no_spec(X).


% Private functions
% Has a spec and is private, should not be flagged
-spec private_with_spec(integer()) -> integer().
private_with_spec(X) -> 
    X + 2.

%% Missing a spec but is private, should not be flagged
private_no_spec(X) -> 
    X + 3.
