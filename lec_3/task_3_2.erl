-module(task_3_2).

%% API
-export([test/0]).

test() ->
  room().

philosopher(Name, _, 0) ->
  io:format("~p is dead~n", [Name]),
  waiterPid ! client_leaving;
philosopher(Name, OwnedForks, MaxLife) ->
  io:format("~p is thinking~n", [Name]),
  timer:sleep(rand:uniform(1000)),

  io:format("~p wants to eat~n", [Name]),
  waiterPid ! { new_client, self(), OwnedForks },

  receive
    start_eating ->
      forksPid ! { reserve_forks, OwnedForks },
      waiterPid ! {client_eating, self(), OwnedForks }
  end,

  io:format("~p is consuming a certain Italian cuisine~n", [Name]),
  timer:sleep(rand:uniform(1000)),

  forksPid ! { release_forks, OwnedForks },
  waiterPid ! client_done,

  philosopher(Name, OwnedForks, MaxLife - 1).

process_waitlist([]) -> false;
process_waitlist([{ClientPid, OwnedForks}|T]) ->
  case checkForks(OwnedForks) of
    true ->
      ClientPid ! start_eating,
      true;
    false -> process_waitlist(T)
  end.

waiter(0, _, [], false) ->
  io:format("Waiter has left~n", []),
  forksPid ! dispose,
  roomPid ! no_clients_left;
waiter(Clients, Eating, Waiting, Busy) ->
  receive
    client_leaving -> waiter(Clients-1, Eating, Waiting, Busy);

    {client_eating, ClientPid, OwnedForks} ->
      Client = {ClientPid, OwnedForks},
      waiter(Clients, Eating+1, Waiting -- [Client], false);

    {new_client, ClientPid, OwnedForks} ->
      Client = {ClientPid, OwnedForks},
      NewWaiting = [Client | Waiting],
      case (not Busy) and (Eating<2) of
        true ->	NewBusy = process_waitlist(NewWaiting);
        false -> NewBusy = Busy
      end,
      waiter(Clients, Eating, NewWaiting, NewBusy);

    client_done -> waiter(Clients, Eating-1, Waiting, process_waitlist(Waiting))
  end.

checkForks(Forks) ->
  forksPid ! {check, Forks, self()},
  receive
    {fork_status, Status} -> Status
  end.

forks(List) ->
  receive
    {check, {L, R}, Requester} ->
      Requester ! {fork_status, lists:member(L, List) andalso lists:member(R, List)},
      forks(List);

    dispose -> io:format("Disposed of forks~n");

    {release_forks, {L, R}} -> forks([L, R | List]);
    {reserve_forks, {L, R}} -> forks(List -- [L, R])
  end.

room() ->
  NameList = ['First', 'Second', 'Third', 'Forth', 'Fifth'],
  ForkList = [{5,1}, {1,2}, {2,3}, {3,4}, {4,5}],

  register(forksPid, spawn(fun() -> forks(lists:seq(1, 5)) end)),
  register(waiterPid, spawn(fun() -> waiter(5, 0, [], false) end)),
  register(roomPid, self()),

  lists:foreach(fun({N, F}) -> spawn(fun() -> philosopher(N, F, 3) end) end, lists:zip(NameList, ForkList)),

  receive
    no_clients_left -> io:format("No clients left~n"), unregister(roomPid), ok
    after 30000 -> unregister(forksPid), unregister(waiterPid), unregister(roomPid), timeout
  end.