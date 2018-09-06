-module(server_SUITE).

-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile([export_all]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type groupName() :: room_manager_SUITE:groupName().
-type proplist() :: room_manager_SUITE:proplist().
-type config() :: room_manager_SUITE:config().

%%%%%%%%%%%%%%%%%%%%%%%%% TEST INITIALIZATION %%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() ->
    [{group, groupName()}].

all() ->
     [
        {group, impossible_interactions},
        {group, basic_interactions}
     ].

-spec groups() ->
    [{groupName(), [sequence], [atom()]}].

groups() ->
    [
        {basic_interactions, [sequence], [
            join_room,
            send_message,
            receive_message
        ]},
        {impossible_interactions, [sequence], [
            cant_send_to_nonexistent_room
        ]}
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%% SUITE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init_per_suite(C :: config()) ->
    config().

init_per_suite(C) ->
    application:ensure_all_started(chat_server),
    application:ensure_all_started(library),
    application:ensure_all_started(chat_client),
    C.

-spec end_per_suite(C :: config()) ->
    config().

end_per_suite(C) ->
    application:stop(chat_server),
    C.

%%%%%%%%%%%%%%%%%%%%%%%%% GROUP INITIALIZATION %%%%%%%%%%%%%%%%%%%%%%%%

-spec init_per_group(groupName(), C :: config()) ->
    config().

init_per_group(basic_interactions, C) ->
    C1 = [
    {       host, "localhost"},
            {port, 8080},
            {id, <<"client1">>},
            {user, <<"Igor">>},
            {message, <<"Hello">>},
            {room, <<"room1">>}
         ] ++ C,
    C1;

init_per_group(impossible_interactions, _C) ->
    C1 = [
            {nonexistent_room_message, {register, <<"Igor">>, <<"">>, <<"noroom">>}},
            {no_room_reply, {error, <<"">>, <<"NO ROOM">>, <<"">>}}
    ],
    C1.


-spec end_per_group(groupName(), C :: config()) ->
    config().

end_per_group(basic_interactions, C) ->
    C;

end_per_group(impossible_interactions, C) ->
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%% BASIC INTERACTIONS %%%%%%%%%%%%%%%%%%%%%%%%%

-spec join_room(C :: config()) ->
    config().

join_room(C) ->
    Host = ?config(host, C),
    Port = ?config(port, C),
    Id   = ?config(id, C),
    connected = client:connect(Host, Port, Id),
    RoomId = ?config(room, C),
    ok = client:join(Id, RoomId),
    C.

-spec send_message(C :: config()) ->
    config().

send_message(C) ->
    Message = ?config(message, C),
    Id = ?config(id, C),
    RoomId = ?config(room, C),
    client:send(Id, Message, RoomId),
    C.

receive_message(C) ->
    Id = ?config(id, C),
    timer:sleep(150), % give server some time to handle and respond
    List = client:get_messages(Id),
    [
        {send_message,<<"Incognito">>,<<"Hello">>,<<"room1">>},
        {joined,<<"Incognito">>,<<>>,<<"room1">>},
        {success,<<>>,<<>>,<<"room1">>}
    ] = List,
    C.


%%%%%%%%%%%%%%%%%%%%%%% IMPOSSIBLE INTERACTIONS %%%%%%%%%%%%%%%%%%%%%%%

-spec cant_send_to_nonexistent_room(C :: config()) ->
    config().

cant_send_to_nonexistent_room(C) ->
    SourceMessage = ?config(nonexistent_room_message, C),
    chat_room:send(SourceMessage, self()),
    ExpectedReply = ?config(no_room_reply, C),
    ExpectedReply = receive_reply(),
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec receive_reply() ->
    protocol:source_message() | nothing.

receive_reply() ->
    receive
        {send, Reply} ->
            Reply
    after 2500 ->
        nothing
    end.

-spec del(Key :: atom(), List :: proplist()) ->
    proplist().

del(Key, List) ->
    proplists:delete(Key, List).
