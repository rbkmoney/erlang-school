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
      io:format("[SV] Proc ~p exited with reason ~p. Restarting...~n", [Worker, Reason]),
      supervise(Fun);
    {'EXIT', _, Reason} ->
      io:format("[SV] Terminating supervisor with reason ~p~n", [Reason]),
      exit(Worker, Reason);
    terminate ->
      io:format("[SV] Supervisor termination requested by user~n"),
      exit(Worker, supervisor_terminated)
  end.