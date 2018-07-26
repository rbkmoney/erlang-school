-module(task_2_3).
-export([fold/3,test/0]).


fold(Fun,Acc,Map) ->
  fold_1(Fun,Acc,maps:to_list(Map)).

%Возможно стоит попробовать сделать это же через гарды
fold_1(_,Acc,[]) ->
  Acc;
fold_1(Fun,Acc,[{Key, Value} | Tail]) ->
  fold_1(Fun,Fun(Key,Value,Acc),Tail).

test() ->
  Fun = fun(K,V,AccIn) when is_list(K) -> AccIn + V end,
  Map = #{"key1" => 1,"key2" => 6, "key7" => 4},
  Acc = 0,
  Sample = maps:fold(Fun,Acc,Map),
  Sample = fold(Fun,0,Map),
  io:format("Test passed~n",[]).
