-module(test_analysis_unused_variables).

-compile(export_all).

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
