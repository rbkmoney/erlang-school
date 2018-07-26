-module(task_2_1).
-export([test/0]).

-define(safe_handle(A), try A catch Class:Err:Stack ->
  {error, {Class,Err,Stack}}
end).

test() ->
  ?safe_handle(erlang:error(bullshit)).
