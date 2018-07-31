-module(task_3_4).

%% API
-export([test/0]).

test() ->
  Parent = self(),
  CacheDB = spawn_link(fun() -> cache_proc(Parent) end),

  receive
    { cache_ok, CacheDB } -> ok
  end,

  Pids = lists:map(fun(N) ->
        spawn_link(fun() ->
          lists:foreach(fun(K) ->
                          task_3_3:puts(N,K),
                          timer:sleep(rand:uniform(1000)),
                          K = task_3_3:gets(N)
                        end, lists:seq(1, 10)),
          Parent ! {ok, self()} end)
        end, lists:seq(1, 100)),

  lists:foreach(fun(P) ->
      receive
        {ok, P} -> ok
      end
    end, Pids),

  CacheDB ! terminate,
  ok.

cache_proc(Parent) ->
  task_3_3:create(),
  io:format("Cache created in ~p~n", [self()]),
  Parent ! {cache_ok, self()},
  receive
    terminate ->
      io:format("Cache disposed in ~p~n", [self()]),
      task_3_3:dispose(),
      ok
  end.
