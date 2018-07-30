-module(task_3_6).
-export([test/0,supervisor/1]).

test() ->
 Fun = fun() -> receive {ok,Msg} -> io:format("New message: ~p~n",[Msg]) end end,
 PID = spawn_link(task_3_6,supervisor,[Fun]),
 io:format("Supervisor PID: ~p~n",[PID]),
 test.

supervisor(Fun) ->
  PID = spawn(Fun),
  io:format("Created process ~p~n",[PID]),
  Ref = erlang:monitor(process,PID),
  receive
    {'DOWN',Ref,_,PID,_} ->
      erlang:demonitor(Ref),
      supervisor(Fun);
    stop ->
      erlang:demonitor(Ref),
      exit(stopped)
  end.
