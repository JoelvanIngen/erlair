-module(test_analysis_unused_variables).

-feature(maybe_expr, enable).

-compile(export_all).

-record(user_info, {
    is_superuser
}).

% 1 unused variable
start() ->
    W = 5,  % Used variable
    X = 0,  % Another used variable
    Y = 1,  % Unused variable
    _Z = 2,  % Ignored variable
    _ = 3,  % Ignored "variable"
    
    X + W.

% None of this should be unused
stream_body_1(Msg, Req=#{pid := Pid}) ->
	cast(Msg, Req),
	receive {data_ack, Pid} -> ok end.
cast(_Msg, _Req) ->
    ok.

% All of this should be unused (3 variables)
stream_body_2(Msg, Req=#{pid := Pid}) -> ok.

% None of this should be unused
update_user_record(UserInfoRecord, [{is_superuser, IsSuperuser} | Rest]) ->
    update_user_record(UserInfoRecord#user_info{is_superuser = IsSuperuser}, Rest).

% Case expression tests
% None of this should be unused
test_case_used_after(TestCase) ->
    case TestCase of
        true -> TestCaseRes = ok;
        false -> TestCaseRes = error
    end,
    TestCaseRes.

% Unused variable (1 variable)
test_case_unused_var(X) ->
    case X of
        1 -> UnusedCaseVar = 10;
        _ -> ok
    end,
    ok.

% None of this should be unused
test_case_var_used_in_branch(Input) ->
    Val = 100,
    case Input of
        check -> Val;
        _ -> 0
    end.

% If expression tests
% None of this should be unused
test_if_used_after(A) ->
    if
        A > 0 -> Status = error;
        true -> Status = ok
    end,
    Status.

% Unused variable (1 variable)
test_if_unused_var(A) ->
    if
        A > 0 -> UnusedIfVar = error;
        true -> ok
    end,
    ok.

% Receive expression tests
% % None of this should be unused
test_receive_used_after() ->
    receive
        {msg, Data} -> Res = Data;
        stop -> Res = stopped
    after 1000 ->
        Res = timeout
    end,
    Res.

% Unused variables: UnusedData, UnusedRecVar (2 variables)
test_receive_unused_var() ->
    receive
        {msg, UnusedData} -> UnusedRecVar = 42
    after 500 ->
        ok
    end,
    ok.

% Try/catch expression tests
% None of this should be unused
test_try_used_after(Input) ->
    try Input of
        {ok, Val} -> Res = Val;
        {error, _} -> Res = err
    catch
        _:_ -> Res = crashed
    after
        ok
    end,
    Res.

% Unused variables: UnusedTryVar, UnusedErr (2 variables)
test_try_unused_var(Input) ->
    try
        UnusedTryVar = Input
    catch
        _:UnusedErr -> ok
    end,
    ok.

% Maybe expression tests
% None of this should be unused
test_maybe_used_after(Input) ->
    maybe
        {ok, X} ?= Input,
        Y = X + 1
    else
        {error, _} -> Y = 0
    end,
    Y.

% Unused variable: UnusedMaybeVar (1 variable)
test_maybe_unused_var(Input) ->
    maybe
        {ok, _} ?= Input,
        UnusedMaybeVar = 0
    end,
    ok.

% begin ... end block tests
% None of this should be unused
test_block_used_after(Input) ->
    begin
        Temp = Input,
        Result = Temp
    end,
    Result.

% Unused variable: UnusedBlockVar (1 variable)
test_block_unused_var() ->
    begin
        UnusedBlockVar = 100
    end,
    ok.

% Anonymous fun tests (creates isolated scope)
% None of this should be unused
test_fun_scope(OuterVar) ->
    F = fun(InnerVar) -> InnerVar + OuterVar end,
    F(10).

% Unused variable: UnusedFunVar (1 variable)
test_fun_unused_var() ->
    F = fun() -> UnusedFunVar = 0, ok end,
    F().

% Comprehension scope tests
% None of this should be unused
test_comprehension_scope(List) ->
    [ Item * 2 || Item <- List ].

% Unused variable: UnusedInComp (1 variable)
test_comprehension_unused(List) ->
    [ Item || Item <- List, UnusedInComp = true ].
