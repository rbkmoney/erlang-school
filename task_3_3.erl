-module(task_3_3).
-export([new/0,lookup/1,insert/2]).

new() ->
  ets:new(database,[set,public,named_table]).

lookup(Key) ->
  try
    [{Key,Value}] = ets:lookup(database,Key),
    Value
  catch
    error:badarg ->
      new(),
      lookup(Key)
  end.


insert(Key, Value) ->
  try
    ets:insert(database,{Key, Value})
  catch
    error:badarg ->
      new(),
      insert(Key,Value)
  end.
