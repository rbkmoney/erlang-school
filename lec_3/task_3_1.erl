-module(task_3_1).

%% API
-export([test/0]).

test() ->
  [2, 4, 6] = pmap(fun(E) -> timer:sleep(rand:uniform(999)), E*2 end, [1,2,3]).

pmap(Func, List) ->
  Pids = lists:map(
    fun(E) ->
      Pid = self(),
      spawn_link(fun() ->
                    Pid ! {ok, self(), Func(E)}
                 end)
    end, List),
  lists:map(
    fun(Pid) -> receive
                {ok, Pid, Result} -> Result
              after 1000 -> timeout
              end
    end,
    Pids
  ).
