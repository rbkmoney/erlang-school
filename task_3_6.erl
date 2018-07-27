-module(task_3_6).
-export([test/0,monitor/1]).

test() ->
 Fun = fun() -> receive {ok,Msg} -> io:format("New message: ~p~n",[Msg]) end end,
 spawn(task_3_6,monitor,[Fun]),
 test.

monitor(Fun) ->
  io:format("Monitor process: ~p~n",[self()]),
  PID = spawn(Fun),
  io:format("Created process ~p~n",[PID]),
  Ref = erlang:monitor(process,PID),
  receive {'DOWN',_,_,_,_} ->
    io:format("Process down, restarting~n"),
    monitor(Fun);
  stop ->
    io:format("Stoping monitor~n"),
    exit(stopped)
  end.
