-module(chatserv_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

-define(AWAIT_DELAY, 50).
-define(AWAIT_RETRIES, 10).

all() ->
    [
        {group, solo_happy},
        {group, solo_failes},
        {group, duo_exchange}
    ].

groups() ->
    [
        {solo_happy, [sequence], [
            join_room,
            set_name,
            send_message
        ]},
        {solo_failes, [sequence], [
            fail_to_set_name,
            fail_to_send_message,
            fail_to_join_room,
            fail_to_double_join
        ]},
        {duo_exchange, [sequence], [
            duo_join,
            duo_names,
            duo_send_same_rooms,
            duo_send_diff_rooms
        ]}
    ].

%%
%% Initializers
%%

init_per_suite(C) ->
    application:ensure_all_started(chatserv),
    application:ensure_all_started(chatcli),
    C.

end_per_suite(C) ->
    application:stop(chatcli),
    application:stop(chatserv),
    C.

init_per_group(solo_happy, C) ->
    make_solo(C);

init_per_group(solo_failes, C) ->
    make_solo(C);

init_per_group(duo_exchange, C) ->
    make_duo(C);

init_per_group(_, C) -> C.

end_per_group(solo_happy, C) ->
    clean_solo(C);

end_per_group(solo_failes, C) ->
    clean_solo(C);

end_per_group(duo_exchange, C) ->
    clean_duo(C);

end_per_group(_, C) -> C.

%%
%% solo_happy
%%

join_room(C) ->
    C1 = get_c1(C),

    ok = join_room_and_await(C1, 1, ok).

set_name(C) ->
    C1 = get_c1(C),

    ok = set_name_and_await(C1, 1, "Test", ok).

send_message(C) ->
    C1 = get_c1(C),

    ok = send_message_and_await(C1, 1, "Test Message", ok),
    _ = timer:sleep(1500),

    1 = length(chatcli_client:get_messages(C1, 1)).

%%
%% solo_fails
%%

fail_to_set_name(C) ->
    C1 = get_c1(C),

    ok = set_name_and_await(C1, 1, "Test", room_not_joined).

fail_to_send_message(C) ->
    C1 = get_c1(C),

    ok = send_message_and_await(C1, 1, "Test Message", room_not_joined).

fail_to_join_room(C) ->
    C1 = get_c1(C),

    ok = join_room_and_await(C1, 999, room_does_not_exist).

fail_to_double_join(C) ->
    C1 = get_c1(C),

    ok = join_room_and_await(C1, 1, ok),
    ok = join_room_and_await(C1, 1, room_already_joined).

%%
%% duo_exchange
%%

duo_join(C) ->
    {C1, C2} = get_c1_c2(C),

    ok = join_room_and_await(C1, 1, ok),

    ok = join_room_and_await(C2, 1, ok),
    ok = join_room_and_await(C2, 2, ok).

duo_names(C) ->
    {C1, C2} = get_c1_c2(C),

    ok = set_name_and_await(C1, 1, "TestUser1", ok),

    ok = set_name_and_await(C2, 1, "TestUser2", ok),
    ok = set_name_and_await(C2, 2, "TestUser2", ok).

duo_send_same_rooms(C) ->
    {C1, C2} = get_c1_c2(C),

    ok = send_message_and_await(C1, 1, "Test Message From User 1", ok),
    ok = send_message_and_await(C2, 1, "Test Message From User 2", ok),

    _ = timer:sleep(1500), %should definitely have time to receive everything

    2 = length(chatcli_client:get_messages(C1, 1)),
    2 = length(chatcli_client:get_messages(C2, 1)),

    0 = length(chatcli_client:get_messages(C1, 2)),
    0 = length(chatcli_client:get_messages(C2, 2)).

duo_send_diff_rooms(C) ->
    {C1, C2} = get_c1_c2(C),

    ok = send_message_and_await(C2, 2, "A message from User 2 user 1 should not see", ok),

    _ = timer:sleep(1500), %should definitely have time to receive everything

    0 = length(chatcli_client:get_messages(C1, 2)),
    1 = length(chatcli_client:get_messages(C2, 2)).

%%
%% Helpers
%%

join_room_and_await(Pid, RoomId, Resp) ->
    ok = chatcli_client:join_room(Pid, RoomId),
    Resp = chatcli_client:await_response(Pid, ?AWAIT_DELAY, ?AWAIT_RETRIES),
    ok.

set_name_and_await(Pid, RoomId, Name, Resp) ->
    ok = chatcli_client:set_name(Pid, RoomId, Name),
    Resp = chatcli_client:await_response(Pid, ?AWAIT_DELAY, ?AWAIT_RETRIES),
    ok.

send_message_and_await(Pid, RoomId, Message, Resp) ->
    ok = chatcli_client:send_message(Pid, RoomId, Message),
    Resp = chatcli_client:await_response(Pid, ?AWAIT_DELAY, ?AWAIT_RETRIES),
    ok.

get_c1(C) ->
    proplists:get_value(client1, C, not_started).

get_c1_c2(C) ->
    {
        proplists:get_value(client1, C, not_started),
        proplists:get_value(client2, C, not_started)
    }.

make_solo(C) ->
    {ok, Client1} = chatcli_sup:start_client("localhost", 8888),
    _ = timer:sleep(500), %some time to connect

    [{client1, Client1} | C].

make_duo(C) ->
    {ok, Client1} = chatcli_sup:start_client("localhost", 8888),
    {ok, Client2} = chatcli_sup:start_client("localhost", 8888),
    _ = timer:sleep(500), %some time to connect

    [{client1, Client1}, {client2, Client2}  | C].

clean_solo(C) ->
    Pid = proplists:get_value(client1, C, not_started),
    chatcli_sup:stop_client(Pid),

    proplists:delete(client1, C).

clean_duo(C) ->
    Pid1 = proplists:get_value(client1, C, not_started),
    Pid2 = proplists:get_value(client2, C, not_started),

    chatcli_sup:stop_client(Pid1),
    chatcli_sup:stop_client(Pid2),

    C1 = proplists:delete(client1, C),
    proplists:delete(client2, C1).