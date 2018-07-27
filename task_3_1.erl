-module(task_3_1).
-export([test/0,pmap/3,particle/3,initialize_collector/0]).

%Параллельный map

test() ->
  F = fun(X) -> X*2 end,
  List = [1,3,5,7,11],
  spawn(task_3_1,pmap,[F,List,[]]),
  test_passed.

% Это не параллельность
pmap(Fun,List,[]) ->
  CollectorId = spawn(task_3_1,initialize_collector,[]),
  pmap(Fun,List,[],CollectorId).
pmap(_,[],PIDs,CollectorId) ->
  CollectorId ! {ready,PIDs};
pmap(Fun,[Head|Tail],PIDs,CollectorId) ->
  PID = spawn(task_3_1,particle,[Fun,Head,CollectorId]),
  pmap(Fun,Tail,[PID] ++ PIDs,CollectorId).


initialize_collector() ->
  receive
    {ready,PIDs} ->
      collect([],PIDs)
    end.

collect(Results,[]) ->
  io:format("~p~n",[lists:reverse(Results)]);
collect(Results,[_ | Others]) ->
    receive {done, Res} ->
        collect([Res] ++ Results,Others)
    end.

particle(Fun,Elem,CollectorId) ->
  CollectorId ! {done,Fun(Elem)}.
