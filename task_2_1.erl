-module(task_2_1).
-export([test/0]).

-define(safe_handle(Arg), try Arg() catch Class:Err:Stack ->
  {error, {Class,Err,Stack}}
end).

test() ->
  F = fun() -> erlang:error(bullshit) end,
  ?safe_handle(F).
