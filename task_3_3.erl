-module(task_3_3).

%% API
-export([test/0, puts/2, gets/1]).

test() ->
  try gets('Hello') catch _ -> io:format("good~n") end,
  puts('Hello', 'World'),
  puts('Hello', 'World2'),
  'World2' = gets('Hello').

create() ->
  ets:new(cache_tb, [public, set, named_table]).

puts(Key, Value) ->
  try
    ets:insert(cache_tb, {Key, Value})
  catch
    error:badarg  -> create(), puts(Key, Value)
  end.

gets(Key) ->
  try
    [{_, Value}] = ets:lookup(cache_tb, Key),
    Value
  catch
    error:badarg  -> create(), gets(Key);
    error:{badmatch, _} -> throw(badarg)
  end.