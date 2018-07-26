-module(task_2_2).

%% API
-export([test/0]).

-define(verbose_handle(Func), try Func() catch Error:Reason -> io:format("Error! ~p ~p~n", [Error, Reason]), throw({Error, Reason}) end).

test() ->
  ?verbose_handle(fun() -> 1 = one end).

