-module(task_2_3).

%% API
-export([test/0]).

test() ->
  Fun = fun(K, V, AccIn) when is_list(K) -> AccIn + V end,
  Map = #{"k1" => 1, "k2" => 2, "k3" => 3},
  try
    fold(fun(_V) -> ok end, 0, #{"correct" => map})
  catch
    error:badarg  -> io:format("Badarg caught as expected~n")
  end,
  try
    fold(fun(_K, _V, _A) -> ok end, 0, [])
  catch
    error:badmap  -> io:format("Badmap caught as expected!~n")
  end,
  fold(Fun, 0, Map).

fold(Func, _, _) when not is_function(Func, 3) ->
  error(badarg);
fold(_, _, Map) when not is_map(Map) ->
  error(badmap, [Map]);
fold(Func, Init, Map) ->
  do_fold(Func, Init, maps:to_list(Map)).

do_fold(_, Acc, []) ->
  Acc;
do_fold(Func, Acc, [{Key, Value}|T]) ->
  do_fold(Func, Func(Key, Value, Acc), T).