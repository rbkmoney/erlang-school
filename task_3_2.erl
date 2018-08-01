-module(task_3_2).
-export([test/0,fork/1,philosopher_/4]).

test() ->
  ForkMap = create_forkMap(),
  ForkPID = spawn_link(phil,fork,[ForkMap]),
  Iter = 20,
  PIDs = [spawn_link(ph,philosopher_,[Item,thinking,ForkPID,Iter]) || Item <- lists:seq(1,5)],
  [send_finish(Elem) || Elem <- PIDs],
  collect_over(PIDs),
  ForkPID ! finish,
  test_over.

  get_forks_nums(Num) ->
    {Num,right(Num)}.

  right(Num) ->
    (Num rem 5) + 1.

  release(Fork,PID) ->
    PID ! {release,Fork}.

  send_finish(PID) ->
    PID ! {finish,self()}.

  collect_over([]) ->
    all_finished;
  collect_over([PID | Rest]) ->
    receive
      {over,PID} ->
        collect_over(Rest)
      end.

create_forkMap() ->
  Nums = lists:seq(1,5),
  Items = [free || _ <- Nums],
  maps:from_list(lists:zip(Nums,Items)).

  fork(ForkMap) ->
    NewMap =
      receive
        {take,Source,Num} ->
          case maps:find(Num,ForkMap) of
            {ok,free} ->
              Source ! accepted,
              maps:update(Num,taken,ForkMap);
            _Else ->
              Source ! denied,
              ForkMap
          end;
        {release,Num} ->
          maps:update(Num,free,ForkMap);
        finish ->
          exit(task_finished)
      end,
    fork(NewMap).

  ask_for_fork(ForkNum,ForkPID) ->
    ForkPID ! {take, self(),ForkNum},
    receive
      accepted ->
        accepted;
      denied ->
        denied
      end.

philosopher_(_,_,_,0) ->
  receive
    {finish,PID} ->
      PID ! {over,self()}
  end;
philosopher_(Num,eating,ForkPID,Iterations) ->
    timer:sleep(rand:uniform(100)),
    {Left,Right} = get_forks_nums(Num),
    release(Right,ForkPID),
    release(Left,ForkPID),
    philosopher(Num,thinking,ForkPID,Iterations-1);

philosopher_(Num,thinking,ForkPID,Iterations) ->
  timer:sleep(rand:uniform(100)),
  philosopher(Num,hungry,ForkPID,Iterations-1);

philosopher_(Num,hungry_retry,ForkPID,Iterations) ->
  timer:sleep(rand:uniform(100)),
  philosopher_(Num,hungry,ForkPID,Iterations);
philosopher_(Num,hungry,ForkPID,Iterations) ->
  {Left,Right} = get_forks_nums(Num),
  NewState =
    case ask_for_fork(Left,ForkPID) of
      accepted ->
        case ask_for_fork(Right,ForkPID) of
          accepted ->
            eating;
          denied ->
            release(Left,ForkPID),
            hungry_retry
        end;
      denied ->
        hungry_retry
      end,
  philosopher(Num,NewState,ForkPID,Iterations-1).

philosopher(Num,NewState,ForkPID,Iterations) ->
  io:format("~p is now ~p ~n",[Num,NewState]),
  philosopher_(Num,NewState,ForkPID,Iterations).
