-module(task_3_2).
-export([test/0,fork/1,philosopher/6]).

test() ->
  ForkMap = create_forkMap(),
  ForkPID = spawn_link(phil,fork,[ForkMap]),
  Iter = 20,
  spawn_link(phil,philosopher,["Adam",1,thinking,[false,false],ForkPID,Iter]),
  spawn_link(phil,philosopher,["Betty",2,thinking,[false,false],ForkPID,Iter]),
  spawn_link(phil,philosopher,["Charlie",3,thinking,[false,false],ForkPID,Iter]),
  spawn_link(phil,philosopher,["Donald",4,thinking,[false,false],ForkPID,Iter]),
  spawn_link(phil,philosopher,["Edna",5,thinking,[false,false],ForkPID,Iter]),
  start.

  left(Num) ->
    case Num == 1 of
      true ->
        5;
      false ->
        Num - 1
      end.


create_forkMap() ->
  Nums = lists:seq(1,5),
  Items = [free || _ <- Nums],
  maps:from_list(lists:zip(Nums,Items)).

  fork(ForkMap) ->
    receive
      {take,Source,Num} ->
        case maps:find(Num,ForkMap) of
          {ok,free} ->
            Source ! accepted,
            fork(maps:update(Num,taken,ForkMap));
          _Else ->
            Source ! denied
        end;
      {release,Num} ->
        fork(maps:update(Num,free,ForkMap))
    end,
    fork(ForkMap).

  ask_for_fork(ForkNum,ForkPID) ->
    ForkPID ! {take, self(),ForkNum},
    receive
      accepted ->
        accepted;
      denied ->
        denied
      end.

  philosopher(_,_,_,_,_,0) ->
    over;
  philosopher(Name,Num,thinking,[HasLeft | HasRight],ForkPID,Iterations) ->
    timer:sleep(rand:uniform(50)),
    Right = Num,
    Left = left(Num),
    case HasLeft of
      false ->
        case ask_for_fork(Left,ForkPID) of
          accepted ->
            philosopher(Name,Num,thinking,[true | HasRight],ForkPID,Iterations - 1);
          denied ->
            philosopher(Name,Num,thinking,[HasLeft | HasRight],ForkPID,Iterations - 1)
        end;
      true ->
        case ask_for_fork(Right,ForkPID) of
          accepted ->
            io:format("~p is now eating~n",[Name]),
            philosopher(Name,Num,eating,[true,true],ForkPID,Iterations-1);
          denied ->
            ForkPID ! {release,Left},
            philosopher(Name,Num,thinking,[false,false],ForkPID,Iterations-1)
        end
      end;

  philosopher(Name,Num,eating,_,ForkPID,Iterations) ->
    timer:sleep(rand:uniform(50)),
    Right = Num,
    Left = left(Num),
    ForkPID ! {release,Right},
    ForkPID ! {release,Left},
    io:format("~p is no longer eating~n",[Name]),
    philosopher(Name,Num,thinking,[false,false],ForkPID,Iterations-1).
