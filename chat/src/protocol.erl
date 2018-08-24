-module(protocol).

-export([encode/2, encode/3, decode/1]).

encode(joined, Username) ->
    {joined, Username};
encode(left, Username) ->
    {left, Username}.

encode(message, Username, Message) ->
    {message, Username, Message}.

decode({message, Username, Message}) ->
    <<Username/binary, ": ", Message/binaryq>>;
decode({left, Username}) ->
    <<Username/binary, " left this chat">>;
decode({joined, Username}) ->
    <<Username/binary, " joined this chat">>.
