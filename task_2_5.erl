-module(task_2_5).

%% API
-export([test/0]).

test() ->
  {ok, IFile} = file:open("task_2_5.txt", read),
  Tokens = readcsv(IFile),
  file:close(IFile),
  transpose(Tokens).

transpose([[]|_]) ->
  [];
transpose(Matrix) ->
  [lists:map(fun(M) -> hd(M) end, Matrix) | transpose(lists:map(fun(M) -> tl(M) end, Matrix))].

readcsv(File) ->
  case file:read_line(File) of
    {ok, Data} -> [string:tokens(Data, ",\n") | readcsv(File)];
    eof        -> []
  end.