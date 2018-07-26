-module(task_2_4).

%% API
-export([test/0]).

-define(DEBUG, 1).

-ifdef(DEBUG).
-define(log(Msg), io:format("{~p:~p} ~p~n", [?MODULE, ?LINE, Msg])).
-else.
-define(log(Msg), []).
-endif.

test() ->
  ?log("Test log!").