-module(task_2_6).

%% API
-export([test/0]).

test() ->
  process("task_2_6.txt").

process(Filename) ->
  Row = readcsv(Filename),
  process_row(Row).

%% process_tokens
process_row(List) ->
  process_row(List, [], List).

process_row([], Stack, _) ->
  lists:reverse(Stack);
process_row([H|T], Stack, List) ->
  process_row(T, [parse_cell(H, List) | Stack], List).

%% parse_cell
parse_cell(Str, List) ->
  parse_cell(string:tokens(Str, " "), [], List).

parse_cell([], Stack, _) ->
  case length(Stack) of
    1 -> hd(Stack);
    _ -> error(badarg)
  end;
parse_cell([Token|Tail], Stack, List) ->
  case Token of
    "$" ++ N -> parse_cell(Tail, [parse_cell(lists:nth(list_to_integer(N), List), List) | Stack], List);
    "+" -> parse_cell(Tail, reduce(fun(A, B) -> A + B end, Stack), List);
    "-" -> parse_cell(Tail, reduce(fun(A, B) -> A - B end, Stack), List);
    "*" -> parse_cell(Tail, reduce(fun(A, B) -> A * B end, Stack), List);
    "/" -> parse_cell(Tail, reduce(fun(A, B) -> A div B end, Stack), List);
    _ -> parse_cell(Tail, [list_to_integer(Token)|Stack], List)
  end.

reduce(F, [A,B|Tail]) ->
  [F(B, A)|Tail];
reduce(_, _) ->
  error(badarg).

readcsv(Filename) ->
  case file:read_file(Filename) of
    {ok, FileBin} -> string:tokens(erlang:binary_to_list(FileBin), ",\n");
    {error, Reason} -> error(Reason)
  end.