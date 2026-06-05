-module(test_analysis_unused_records).

-export([start/0]).

%% ====================================================================
%% Record Declarations
%% ====================================================================

%% all fields of 'used_rec' are referenced
-record(used_rec, {
    a,
    b
}).

%% Testing single unused field
%% 'partially_used_rec' is referenced, but 'field_c' never used
-record(partially_used_rec, {
    field_a,
    field_b,
    field_c
}).

%% Testing multiple unused fields
%% `field_c` and `field_d` are never used
-record(partially_used_rec_2, {
    field_a,
    field_b,
    field_c,
    field_d
}).

%% 'unused_rec' is not used at all
-record(unused_rec, {
    x,
    y
}).

%% ====================================================================
%% Code using the records
%% ====================================================================

start() ->
    %% Instantiate records
    _ = #used_rec{a = 0, b = 1},
    R = #partially_used_rec{field_a = 0},
    
    %% Uuses 'used_rec' and 'field_b'
    #partially_used_rec{field_b = B} = R,

    S = #partially_used_rec_2{field_a = 0, field_b = 1},
    
    B.
