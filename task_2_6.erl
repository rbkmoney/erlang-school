-module(task_2_6).
-export([test/0]).

%Одномерный эксель в обратной польской нотации
test() ->
  Problem = ["1 9 *;2;$1 $2 *;4 24 +;5 3 +;6 2 /"],
  io:format("Input: ~p~n",[Problem]),
  Map = create_cell_map(Problem),
  iterate(Map,1,maps:size(Map)).

iterate(Map,Acc,Max) when Acc > Max ->
  Map;
iterate(Map,Acc,Max) ->
  NewMap = maps:update(Acc,count(maps:get(Acc,Map),Map),Map),
  iterate(NewMap,Acc+1,Max).

create_cell_map(Problem) ->
  Values = string:split(Problem,";",all),
  Keys = lists:seq(1,length(Values)),
  maps:from_list(lists:zip(Keys,Values)).


rpn_fold(Acc,[],_) ->
  Acc;
rpn_fold(Acc,[Head | Tail],Map) ->
  rpn_fold(rpn(Head,Acc,Map),Tail,Map).

count(Problem,Map) ->
  case is_integer(Problem)  of
    true ->
    Res = Problem;
  _Else ->
    Normalized = string:tokens(Problem," "),
    [Res] = rpn_fold([],Normalized,Map)
  end,
  Res.

proceed_link(Link,Map) ->
  count(maps:get(list_to_integer(Link),Map),Map).


rpn("$" ++ Link,Stack,Map) -> [proceed_link(Link,Map)] ++ Stack;
rpn("+",[N1,N2 | Stack],_) -> [N1 + N2] ++ Stack;
rpn("-",[N1,N2 | Stack],_) -> [N1 - N2] ++ Stack;
rpn("*",[N1,N2 | Stack],_) -> [N1 * N2] ++ Stack;
rpn("/",[N1,N2 | Stack],_) -> [N2 div N1] ++ Stack;
rpn(X, Stack,_) -> [list_to_integer(X)]++ Stack.
