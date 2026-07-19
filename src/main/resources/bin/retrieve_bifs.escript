#!/usr/bin/env escript

main(_) ->
    Exports = erlang:module_info(exports),
    
    % Auto-imported BIFs
    Bifs = [{F, A} || {F, A} <- Exports, erl_internal:bif(F, A)],
    io:format("BIFs (~p):~n", [length(Bifs)]),
    print_list(lists:sort(Bifs)),
    io:format("~n"),

    %Guard BIFs
    Guards = [{F, A} || {F, A} <- Exports, erl_internal:guard_bif(F, A)],
    io:format("Guards (~p):~n", [length(Guards)]),
    print_list(lists:sort(Guards)),
    io:format("~n"),

    % Type Tests
    Types = [{F, A} || {F, A} <- Exports, erl_internal:type_test(F, A)],
    io:format("Types (~p):~n", [length(Types)]),
    print_list(lists:sort(Types)),
    io:format("~n"),
    
    ok.

print_list(List) ->
    [io:format("    <\"~p\", ~p>,~n", [F, A]) || {F, A} <- List],
    ok.
