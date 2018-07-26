-module(task_2_2).

%% API
-export([test/0]).

-define(verbose_handle(Func), try Func catch Error:Reason -> io:format("Error! ~p ~p~n", [Error, Reason]), erlang:raise(Error, Reason, erlang:get_stacktrace()) end).

test() ->
  ?verbose_handle(1 = one).

