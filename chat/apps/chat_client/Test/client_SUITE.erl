-module(client_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

all() ->
    [
        {group, basic_interactions}
    ].

groups() ->
    [
        {basic_interactions, [sequence], [
            connect_to_websocket,
            set_username,
            join_room,
            send_message,
            receive_message
        ]}
    ].

init_per_suite(C) ->
    C1 = [{host, "localhost"},
            {port, 8080},
            {id, client1},
            {room, room1},
            {user, <<"Igor">>},
            {message, <<"Hello">>}
         ] ++ C,
    application:ensure_all_started(chat_server),
    application:start(chat_server),
    application:ensure_all_started(chat_client),
    application:start(chat_client),
    C1.

end_per_suite(C) ->
    application:stop(chat_client),
    %application:stop(chat_server),
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%% BASIC INTERACTIONS %%%%%%%%%%%%%%%%%%%%%%%%%

connect_to_websocket(C) ->
    Host = get(host, C),
    Port = get(port, C),
    Id   = get(id, C),
    ct:print("Host: ~p, Port: ~p, Id: ~p",[Host, Port, Id]),
    connected = client:connect(Host, Port, Id),
    C.

set_username(C) ->
    Username = get(user, C),
    Id = get(id, C),
    Username = client:set_username(Id, Username),
    C.

join_room(C) ->
    RoomId = get(room, C),
    Id = get(id, C),
    ok = client:join(Id, RoomId),
    C.

send_message(C) ->
    Message = get(message, C),
    Id = get(id, C),
    RoomId = get(room, C),
    client:send(Id, Message, RoomId),
    C.

receive_message(C) ->
    Message = get(message, C),
    Id = get(id, C),
    timer:sleep(150), %give server some time to handle and respond
    List = client:get_messages(Id),
    [{_, _, Message, _}, _, _] = List,
    C.

get(Key, List) ->
    proplists:get_value(Key, List).
