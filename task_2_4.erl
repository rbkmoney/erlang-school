-module(task_2_4)
-export([test/0])
-define(debug, debug.).

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X),true).
-endif.

test() ->
  ?LOG(test_began).
