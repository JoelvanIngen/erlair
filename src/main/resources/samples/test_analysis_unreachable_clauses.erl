-module(test_analysis_unreachable_clauses).

-feature(maybe_expr, enable).

-compile(export_all).

-record(rec, {
    a,
    b
}).

-record(rec_2, {
    c,
    d
}).

% Catch-all variable in function declaration
catch_all_fn(_X) -> ok;
catch_all_fn(0) -> unreachable.

% Catch-all variable in case block
case_test(Val) ->
    case Val of
        _ -> ok;
        {ok, _Result} -> unreachable;
        _ -> unreachable
    end.

% Other language constructs:
% Receive block without `after`
receive_test_1() ->
    receive
        _ -> ok;
        {ok, _} -> unreachable
    end.

% Receive block with `after`
receive_test_2() ->
    receive
        _ -> ok;
        {ok, _} -> unreachable
    after 1 ->
        timeout
    end.

% `if`
if_test(X) ->
    if
        X >= 0 -> ok;
        X > 10 -> unreachable
    end.

% Anonymous function
fun_test() ->
    F = fun
        (_) -> ok;
        (0) -> unreachable
    end,
    F(0).

% Named anonymous function
named_fun_test() ->
    F = fun _Rec(_) -> ok;
            _Rec(0) -> unreachable
    end,
    F(0).

% `maybe` with `else`
maybe_test(X) ->
    maybe
        {ok, Y} ?= X,
        Y
    else
        _ -> ok;
        _ -> unreachable
    end.

% `try ... of` case clauses
try_test_1(X) ->
    try X of
        _ -> ok;
        {ok, _} -> unreachable
    catch
        _:_ -> error
    end.

% `try ... catch` exception clauses
try_test_2(X) ->
    try X()
    catch
        _Y:_Z -> ok;
        _ -> unreachable
    end.

% Multiple variable matches
non_linear_fn(_X) -> ok;
non_linear_fn(_Y) -> unreachable;
non_linear_fn(_) -> unreachable.

non_linear_fn_2(_, _Y) -> ok;
non_linear_fn_2(_, _X) -> unreachable;
non_linear_fn_2(_, _) -> unreachable.

% Tuples
tuples_fn({_A, _B}) -> ok;
tuples_fn({1, 2}) -> unreachable;
tuples_fn(_X) -> ok;
tuples_fn(_) -> unreachable.

tuples_fn_2({ok, _}) -> ok;
tuples_fn_2({ok, ready}) -> unreachable;
tuples_fn_2({_, ready}) -> ok.

% Lists
lists_fn([_ | _]) -> ok;
lists_fn([1, 2, 3]) -> unreachable;
lists_fn(_) -> ok.

% Maps
maps_fn(#{key1 := _}) -> ok;
maps_fn(#{key1 := _, key2 := _}) -> unreachable;
maps_fn(#{key2 := _}) -> ok.

% Guards
guards_fn_1(X) when X >= 0 -> ok;
guards_fn_1(X) when X > 10 -> unreachable.

guards_fn_2(X) when X >= 0 -> ok;
guards_fn_2(Y) when Y > 10 -> unreachable.

guards_fn_3(X) when X >= 0 -> ok;
guards_fn_3(X) when X >= 10 -> unreachable.

guards_fn_4(X) when X > 0 -> ok;
guards_fn_4(X) when X > 10 -> unreachable.

guards_fn_5(X) when X > 10 -> ok;
guards_fn_5(X) when X >= 11 -> unreachable.

guards_fn_6(X) when X =< 10 -> ok;
guards_fn_6(X) when X < 5 -> unreachable.

guards_fn_7(X) when X =< 10 -> ok;
guards_fn_7(X) when X =< 5 -> unreachable.

guards_fn_8(X) when X < 10 -> ok;
guards_fn_8(X) when X < 5 -> unreachable.

guards_fn_9(X) when X < 10 -> ok;
guards_fn_9(X) when X =< 9 -> unreachable.

% Built-in type guards
literal_guard_integer(X) when is_integer(X) -> ok;
literal_guard_integer(5) -> unreachable.

literal_guard_atom(X) when is_atom(X) -> ok;
literal_guard_atom(hello) -> unreachable.

literal_guard_float(X) when is_float(X) -> ok;
literal_guard_float(1.1) -> unreachable.

literal_guard_number_int(X) when is_number(X) -> ok;
literal_guard_number_int(10) -> unreachable.

literal_guard_number_float(X) when is_number(X) -> ok;
literal_guard_number_float(1.1) -> unreachable.

literal_guard_list_fn(X) when is_list(X) -> ok;
literal_guard_list_fn([]) -> ok.

literal_guard_binary_fn(X) when is_binary(X) -> ok;
literal_guard_binary_fn(<<"test">>) -> ok.

% Nested type guard
nested_var_to_lit_tuple({X}) when is_integer(X) -> ok;
nested_var_to_lit_tuple({1}) -> unreachable.

nested_var_to_lit_list([X]) when is_integer(X) -> ok;
nested_var_to_lit_list([1]) -> unreachable.

% Map variables
map_var_prop(#{key1 := X}) when is_integer(X) -> ok;
map_var_prop(#{key1 := 1}) -> unreachable.

map_nil_string(#{key1 := []}) -> ok;
map_nil_string(#{key1 := ""}) -> unreachable.

% Duplicate pattern
dup_fn(ok) -> ok;
dup_fn(ok) -> unreachable.

% Type inheritance
type_hierarchy_fn_1(X) when is_number(X) -> ok;
type_hierarchy_fn_1(X) when is_integer(X) -> unreachable.

type_hierarchy_fn_2(X) when is_bitstring(X) -> ok;
type_hierarchy_fn_2(X) when is_binary(X) -> unreachable.

% Record with unspecified field
record_subsumption_fn(#rec{}) -> ok;
record_subsumption_fn(#rec{a = 1}) -> unreachable.

% Generic binary
binary_fn_1(<<_/binary>>) -> ok;
binary_fn_1(<<"test">>) -> unreachable.

% Binary
bitstring_fn_2(<<_X:8, _Y:8>>) -> ok;
bitstring_fn_2(<<1, 2>>) -> unreachable.

% % Guard that is always false
% Could be implemented but would make the code quite messy for a case that no one will realistically ever write
% failing_guard_fn(_) when false -> unreachable.

% Empty string list
string_list_fn_1([]) -> ok;
string_list_fn_1("") -> unreachable.

string_list_fn_2("") -> ok;
string_list_fn_2([]) -> unreachable.

% Inline pattern match
match_in_pattern_fn(_X = {ok, _}) -> ok;
match_in_pattern_fn({ok, 1} = _Y) -> unreachable.

% Second clause is reachable
record_mismatch_fn(#rec{}) -> ok;
record_mismatch_fn(#rec_2{}) -> ok.

% Using record fields to avoid warning
extra() -> 
    _A = #rec{b = 0},
    _B = #rec_2{c = 0, d = 1}.
