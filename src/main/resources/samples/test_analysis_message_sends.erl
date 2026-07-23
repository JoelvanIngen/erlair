-module(test_analysis_message_sends).

-compile(export_all).

% Starts system and sends message
start() ->
    Pid = spawn(test_messaging, sender, []),
    register(test_receiver, Pid),
    
    % This should show up in report
    test_receiver ! start_signal,
    ok.

% Sends different kinds of messages
sender() ->
    % This should show up in report
    logger ! {info, "Sender process is active"},
    
    % This should be ignored in report (sending to variable instead of hardcoded value)
    Self = self(),
    Self ! self_msg,
    ok.

% External destination
run(Dest) ->
    % This should be ignored in report (sending to variable instead of hardcoded value)
    Dest ! run_complete.
