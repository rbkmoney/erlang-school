-module(task_2_1).

%% API
-export([test/0]).

-define(safe_handle(Func), try Func() catch Error:Reason -> {error, Error, Reason} end).

test() ->
  ?safe_handle(fun() -> 1 = one end).