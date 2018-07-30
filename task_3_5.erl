-module(task_3_5).

%% API
-export([test/0]).

test() ->
  Bar = spawn_link(fun() -> proc_bar() end),
  spawn_link(fun() -> proc_foo(Bar, 10) end),
  test_exited.

proc_foo(Bar, 0) -> Bar ! end_bar;
proc_foo(Bar, Ct) ->
  io:format("foo "),
  Bar ! { request_bar, self() },
  receive
    { receive_bar, Bar } -> proc_foo(Bar, Ct - 1)
  end.

proc_bar() ->
  receive
    { request_bar, Resp } -> io:format("bar~n"), Resp ! { receive_bar, self() }, proc_bar()
  end.