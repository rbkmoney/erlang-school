-module(task_3_5).

%% API
-export([test/0]).

test() ->
  #{ username := "user"} = get_user_data().


get_user_data() ->
  Parent = self(),
  Bar = spawn(fun() -> dubious_api_handler(Parent) end),
  erlang:monitor(process, Bar),
  Bar ! get_user_data,
  receive
    {'DOWN', _, process, Bar, _} -> get_user_data();
    {ok, Data } -> Data
  end.

dubious_api_handler(Parent) ->
  receive
    get_user_data ->
      case (rand:uniform(10) rem 2 == 0) of
        true -> io:format("Request failed!~n"), exit(self(), request_failed);
        false -> Parent ! { ok, #{ username => "user" , email => "user@domain.root" } }
      end
  end.
