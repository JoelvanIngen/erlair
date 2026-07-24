-module(test_analysis_naming_conventions).

-export(['DoSomething'/0, do_something_else/0]).

% Records
% Should be flagged
-record('BadRecordName', {
    field_a
}).

% Should not be flagged
-record(good_record_name, {
    field_b
}).

% Functions
% Should be flagged
'DoSomething'() ->
    % Should not be flagged (uppercases in variables are fine)
    Variable = 5,
    _ = #good_record_name{field_b = Variable},
    ok.

% Should not be flagged
do_something_else() ->
    _ = #'BadRecordName'{field_a = 1},
    ok.
