-module(task_3_4).

%% API
-export([test/0]).

test() ->
  lists:foreach(fun(N) ->
        spawn_link(fun() -> lists:foreach(
                      fun(K) -> task_3_3:puts(N,K), timer:sleep(rand:uniform(1000)), task_3_3:gets(N) end, lists:seq(1, 1000))
                   end)
        end, lists:seq(1, 1000)).