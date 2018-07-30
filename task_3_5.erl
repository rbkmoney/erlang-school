-module(task_3_5).

%% API
-export([test/0]).

test() ->
  Parent = self(),
  Bar = spawn(fun() -> dubious_api(Parent) end),
  erlang:monitor(process, Bar),
  Bar ! get_user_data,
  receive
    {'DOWN', _, process, Bar, _} -> test();
    {ok, Code } -> Code
  end.

dubious_api(Parent) ->
  receive
    get_user_data ->
      case (rand:uniform(10) div 2 == 0) of
        true -> io:format("Request failed!~n"), exit(self(), 503);
        false -> Parent ! { ok, 200 }
      end
  end.
