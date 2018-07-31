-module(task_3_6).
-export([test/0,supervisor/1]).

test() ->
 Fun = fun() -> receive {ok,Msg} -> io:format("New message: ~p~n",[Msg]) end end,
 PID = spawn_link(task_3_6,supervisor,[Fun]),
 io:format("Supervisor PID: ~p~n",[PID]),
 test.

supervisor(Fun) ->
  process_flag(trap_exit,true),
  PID = spawn_link(Fun),
  io:format("Created process ~p~n",[PID]),
  receive
    {'EXIT',PID,_} ->
      supervisor(Fun);
    {'EXIT',_,Reason} ->
      exit(Reason);
    stop ->
      exit(stopped)
  end.
