-module(server_SUITE).

-include_lib("common_test/include/ct.hrl").
%
-compile([export_all]).
%
all() ->
     [
        {group, impossible_interactions},
        {group, basic_interactions}
     ].

groups() ->
    [
        {basic_interactions, [sequence], [
            server_workflow
        ]},
        {impossible_interactions, [sequence], [
            cant_send_to_nonexistent_room
        ]}
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% _ PER SUITE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(C) ->
    application:ensure_all_started(chat_server),
    application:ensure_all_started(library),
    application:start(chat_server),
    application:start(library),
    C.

end_per_suite(C) ->
    application:stop(chat_server),
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% _ PER GROUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_group(basic_interactions, C) ->
    C1 = [
            {room, room1},
            {registration_message, {register, <<"Igor">>, <<"">>, room1}},
            {registration_reply, {success, <<"">>, <<"">>, room1}},
            {joined_reply, {joined, <<"Igor">>, <<"">>, room1}},
            {send_message, {send_message, <<"Igor">>, <<"Hello">>, room1}},
            {termination_message, {error, <<"">>, <<"Room is terminated">>, room1}}
         ] ++ C,
    C1;

init_per_group(impossible_interactions, _C) ->
    C1 = [
            {nonexistent_room_message, {register, <<"Igor">>, <<"">>, noroom}},
            {no_room_reply, {error, <<"">>, <<"NO ROOM">>, no_room}}
    ],
    C1.

end_per_group(basic_interactions, C) ->
    C;

end_per_group(impossible_interactions, C) ->
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%% BASIC INTERACTIONS %%%%%%%%%%%%%%%%%%%%%%%%%

server_workflow(C) -> % Need to send from 1 pid, so I putted all functions inside
    join_room(C),
    send_message(C),
    get_termination_message(C),
    C.

join_room(C) ->
    {Event, Username, Message, Room} = get(registration_message, C),
    Json = protocol:encode(Event, Username, Message, Room),
    chat_room:send(Json, self()),
    ct:print("PID: ~p~n", [self()]),
    ExpectedReply1 = get(registration_reply, C),
    ExpectedReply2 = get(joined_reply, C),
    ExpectedReply1 = receive_reply(),
    ExpectedReply2 = receive_reply(),
    C.

send_message(C) ->
    {Event, Username, Message, Room} = get(send_message, C),
    Json = protocol:encode(Event, Username, Message, Room),
    chat_room:send(Json, self()),
    ct:print("PID: ~p~n", [self()]),
    ExpectedReply = {Event, Username, Message, Room},
    ExpectedReply = receive_reply(),
    C.

get_termination_message(C) ->
    ExpectedReply = get(termination_message, C),
    Room = get(room, C),
    room_manager:delete_room(Room),
    ExpectedReply = receive_reply(),
    C.

%%%%%%%%%%%%%%%%%%%%%%% IMPOSSIBLE INTERACTIONS %%%%%%%%%%%%%%%%%%%%%%%

cant_send_to_nonexistent_room(C) ->
    {Event, Username, Message, Room} = get(nonexistent_room_message, C),
    Json = protocol:encode(Event, Username, Message, Room),
    chat_room:send(Json, self()),
    ExpectedReply = get(no_room_reply, C),
    ExpectedReply = receive_reply(),
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

receive_reply() ->
    receive
        {send, Reply} ->
            protocol:decode(Reply)
    after 2500 ->
        nothing
    end.

get(Key, List) ->
    proplists:get_value(Key, List).

del(Key, List) ->
    proplists:delete(Key, List).
