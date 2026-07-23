-module(test_analysis_process_spawns).

-compile(export_all).

start() ->
    %% Local static MFA
    spawn(?MODULE, worker, [mfa]),
    
    %% Distributed static MFA
    spawn('node@host', ?MODULE, worker2, [nmfa]),
    
    %% Local fun reference
    spawn(fun local_worker/0),
    
    %% Remote fun reference
    spawn(fun ?MODULE:remote_worker/1),
    
    %% Anonymous fun
    spawn(fun() -> anon_worker() end),
    
    %% Dynamic MFA
    Mod = ?MODULE,
    spawn(Mod, worker, [dynamic]),
    
    ok.

%% Target functions
worker(_) -> ok.
worker2(_) -> ok.
local_worker() -> ok.
remote_worker(_) -> ok.
anon_worker() -> ok.
