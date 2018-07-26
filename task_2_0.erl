-module(task_2_0).

%% API
-export([test/0]).

test() ->
  [1, 2, 3, 4] = seq(1, 4),
  [1, 3] = seq(1, 4, 2),
  [0, 2, 4, 6, 8] = seq(0, 8, 2),
  [1, 6] = seq(1, 10, 5),
  [1, 3] = filter(
    fun(Elem) ->
      Elem rem 2 == 1
      end,
    [1, 2, 3]
  ),
  [{1, 3}, {2, 2}, {3, 1}] = zip([1, 2, 3], [3, 2, 1]),
  try
      zip([1,2], [1])
  catch
      error:function_clause  -> io:format("zip error handling works!~n")
  end .

seq(From, To) ->
  seq(From, To, 1).
seq(From, To, Step) when To < From - Step, Step > 0 ->
  error(badarg);
seq(From, To, Step) when To > From - Step, Step < 0 ->
  error(badarg);
seq(From, To, Step) when From > To, Step > 0  ->
  [];
seq(From, To, Step) when From < To, Step < 0 ->
  [];
seq(From, To, Step) ->
  [From | seq(From + Step, To, Step)].

filter(_Func, []) ->
  [];
filter(Func, [H|T]) ->
  case Func(H) of
    true -> [H|filter(Func, T)];
    false -> filter(Func, T)
  end.

zip([], []) ->
  [];
zip(List1, List2) when erlang:length(List1) == erlang:length(List2) ->
  [H1|T1] = List1,
  [H2|T2] = List2,
  [{H1, H2} | zip(T1, T2)].
