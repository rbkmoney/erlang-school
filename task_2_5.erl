-module(task_2_5).
-export([test/0]).

test() ->
  {ok,Content} = file:open("matrix.txt",read),
  Matrix = read_integers(Content,[]),
  transpose(Matrix).


string_list_to_int_list(List) ->
  string_list_to_int_list(List,[]).
string_list_to_int_list([],Acc) ->
  lists:reverse(Acc);
string_list_to_int_list([Head | Tail],Acc) ->
  string_list_to_int_list(Tail,[list_to_integer(Head) | Acc]).

%Нашел реализацию на stackoverflow и доработал под свои нужды
read_integers(Device,Acc) ->
  case file:read_line(Device) of
    eof ->
        lists:reverse(Acc);
    {ok, Line} ->
        StrNumbers = string:split(string:strip(Line, right, 10),",",all),
        read_integers(Device,[string_list_to_int_list(StrNumbers)| Acc])
  end.


transpose(Matrix) ->
  transpose(Matrix,[]).

transpose([[],[]],Acc) ->
  lists:reverse(Acc);
transpose(Matrix,[[]]) ->
  transpose(delete_heads(Matrix),get_heads(Matrix));
transpose(Matrix,Acc) ->
  transpose(delete_heads(Matrix),[get_heads(Matrix)| Acc]).

get_heads(Matrix) ->
  get_heads(Matrix,[]).

get_heads([],Acc) ->
  lists:reverse(Acc);
get_heads([[Head | _] | Rest],Acc) ->
  get_heads(Rest,[Head | Acc]).

delete_heads(Matrix) ->
  delete_heads(Matrix,[]).

delete_heads([],Acc) ->
  lists:reverse(Acc);
delete_heads([[Head | Tail]| Rest],Acc) ->
  delete_heads(Rest,[Tail | Acc]).
