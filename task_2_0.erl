-module(task_2_0).
-export([seq/2,seq/3,filter/2,zip/2,test/0]).

seq(Start,Finish) ->
  seq(Start,Finish,1).

seq(_,_,Step) when Step == 0 ->
  erlang:error(badarg,[Step]);
seq(Start,Finish,Step) when Start > Finish, Step > 0 ->
  erlang:error(badarg,[Start,Finish,Step]);
seq(Start,Finish,Step) when Start < Finish, Step < 0 ->
  erlang:error(badarg,[Start,Finish,Step]);
seq(Start,Finish,Step) ->
  seq(Start,Finish,Step,[]).

seq(Start,Finish,Step,Acc) when Start > Finish, Step > 0->
  lists:reverse(Acc);
seq(Start,Finish,Step,Acc) when Start < Finish, Step < 0->
  lists:reverse(Acc);
seq(Start,Finish,Step,Acc) ->
  seq(Start + Step,Finish,Step,[Start | Acc]).


filter(Fun,List) ->
  filter(Fun,List,[]).

filter(_,[],Acc) ->
  lists:reverse(Acc);
filter(Fun, [Head | Tail],Acc) ->
  case Fun(Head) of
    true ->
      filter(Fun,Tail,[Head | Acc]);
    false ->
      filter(Fun,Tail,Acc)
  end.

%Написанный zip поддерживает списки разной длины
zip(List1, List2) ->
  zip(List1,List2,[]).

zip([],_,Result) ->
  lists:reverse(Result);
zip(_,[],Result) ->
  lists:reverse(Result);
zip([Head1 | Tail1],[Head2 | Tail2],Result) ->
  zip(Tail1,Tail2,[{Head1,Head2} | Result]).


test() ->
  %Seq test
  Sample = lists:seq(1,10),
  Sample = seq(1,10),
  Sample1 = lists:seq(1,-10,-2),
  Sample1 = seq(1,-10,-2),
  %Filter test
  GT5 = fun(X) -> X > 5 end,
  SeqSample = lists:filter(GT5,Sample),
  SeqSample = filter(GT5,Sample),
  %Zip test
  A = [1,2,3],
  B = [a,b,c],
  ZipSample = lists:zip(A,B),
  ZipSample = zip(A,B),
  io:format("Test passed~n").
