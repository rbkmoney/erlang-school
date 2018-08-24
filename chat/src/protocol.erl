-module(protocol).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([encode/2]).
-export([encode/3]).
-export([decode/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec encode
    (joined, chat_server:message()) ->
        {joined, chat_server:message()};
    (left, chat_server:username()) ->
        {left, chat_server:username()}.

encode(joined, Username) ->
    {joined, Username};
encode(left, Username) ->
    {left, Username}.

-spec encode(message, chat_server:username(), chat_server:message()) ->
    {message, chat_server:username(), chat_server:message()}.
encode(message, Username, Message) ->
    {message, Username, Message}.

-spec decode(
    {message, chat_server:username(), chat_server:message()}) ->
        binary();
    {left, chat_server:username()} ->
        binary().
decode({message, Username, Message}) ->
    <<Username/binary, ": ", Message/binary>>;
decode({left, Username}) ->
    <<Username/binary, " left this chat">>;
decode({joined, Username}) ->
    <<Username/binary, " joined this chat">>.
