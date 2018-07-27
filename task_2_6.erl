-module(task_2_6).
-export([test/0]).

%Одномерный эксель в обратной польской нотации
test() ->
  Problem = ["1;2;$1 2 +;4;5 3 +;6"],
  Map = create_cell_map(Problem),
  iterate(Map,1,maps:size(Map)).


iterate(Map,Acc,Max) when Acc > Max ->
  io:format("~p~n",[Map]),
  over;
iterate(Map,Acc,Max) ->
  Ans = string:find(maps:get(Acc,Map),"$"),
    if Ans == nomatch ->
      NewMap = maps:update(Acc,count(maps:get(Acc,Map)),Map);
      true ->
        [_| Tail] = maps:get(Acc,Map),
        [LinkStr] = string:tokens(Tail," "),
        Link = list_to_integer(LinkStr),
        NewMap = maps:update(Acc,count(maps:get(Link,Map)),Map),
        io:format("~p~n",[Link])
    end,
  iterate(NewMap,Acc+1,Max).



create_cell_map(Problem) ->
  Values = string:split(Problem,";",all),
  Keys = lists:seq(1,length(Values)),
  Map = maps:from_list(lists:zip(Keys,Values)).



count(Problem) ->
  Normalized = string:tokens(Problem," "),
  [Res] = lists:foldl(fun rpn/2,[],Normalized),
  Res.



rpn("+",[N1,N2 | Stack]) -> [N1 + N2 | Stack];
rpn("-",[N1,N2 | Stack]) -> [N1 -  N2 | Stack];
rpn("*",[N1,N2 | Stack]) -> [N1 * N2 | Stack];
rpn("/",[N1,N2 | Stack]) -> [N1 div N2 | Stack];
rpn(X, Stack) -> [list_to_integer(X) | Stack].
