-module(task_3_1).
-export([test/0,pmap/2,particle/3]).
 %Параллельный map
 test() ->
  F = fun(X) -> X * 2 end,
  List = [1,3,5,7,11],
  Sample = lists:map(F,List),
  Sample = pmap(F,List),
  test_passed.

pmap(Fun,List) ->
  PIDs = [spawn_link(task_3_1,particle,[Fun,Element,self()]) || Element <- List],
  [collect(Elem) || Elem <- PIDs].

collect(PID) ->
  receive
    {done,PID,Val} ->
      Val
  end.

 particle(Fun,Elem,CollectorId) ->
  CollectorId ! {done,self(),Fun(Elem)}.
