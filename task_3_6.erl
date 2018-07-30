-module(task_3_6).

%% API
-export([test/0]).


test() ->
  Supervisor = spawn_link(fun() -> supervise(fun() -> timer:sleep(rand:uniform(1000)), exit(self(), test_reason) end) end),
  timer:sleep(10000),
  Supervisor ! terminate,
  ok.


supervise(Fun) ->
  process_flag(trap_exit, true),
  Worker = spawn_link(Fun),
  io:format("[SV] Watching proc ~p~n", [Worker]),
  receive
    {'EXIT', Worker, Reason} ->
      io:format("[SV] Proc ~p exited with ~p. Restarting...~n", [Worker, Reason]),
      process_flag(trap_exit, false),
      supervise(Fun);
    terminate ->
      io:format("[SV] terminating supervisor...~n"),
      exit(Worker, supervisor_terminated),
      process_flag(trap_exit, false)
  end.