-module(task_3_3).

%% API
-export([
  test/0,
  puts/2,
  gets/1,
  create/0,
  dispose/0
]).

test() ->
  create(),

  try gets('Hello') catch _ -> io:format("good~n") end,
  puts('Hello', 'World'),
  puts('Hello', 'World2'),
  'World2' = gets('Hello'),

  dispose().

create() ->
  ets:new(cache_tb, [public, set, named_table]).

puts(Key, Value) ->
  ets:insert(cache_tb, {Key, Value}).

gets(Key) ->
  try
    [{_, Value}] = ets:lookup(cache_tb, Key),
    Value
  catch
    error:{badmatch, _} -> throw(badarg)
  end.


dispose() ->
  ets:delete(cache_tb).