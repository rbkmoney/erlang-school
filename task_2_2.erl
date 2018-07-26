-module(task_2_2).
-export([test/0]).
-define(verbose_handle(Arg), try Arg() catch Class:Err  ->
  io:format("~p ~p~n",[Class,Err]),
  erlang:raise(Class,Err,erlang:get_stacktrace())
end).

test() ->
  F = fun() -> erlang:error(bullshit) end,
  ?verbose_handle(F).
