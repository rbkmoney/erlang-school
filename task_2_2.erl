-module(task_2_2).
-export([test/0]).
-define(verbose_handle(Arg), try Arg() catch _:Err1:Stack1  ->
  erlang:display(Stack1),
  erlang:throw(Err1)
end).

test() ->
  F = fun() -> erlang:error(bullshit) end,
  ?verbose_handle(F).
