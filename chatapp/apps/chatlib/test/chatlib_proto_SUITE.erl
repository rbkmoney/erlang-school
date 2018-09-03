-module(chatlib_proto_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

all() ->
    [
        proto_encode_decode,
        json_encode,
        json_decode
    ].

json_decode(_) ->
    {server_response, global, ok} =
        chatlib_proto:decode(<<"{\"type\":\"server_response\",\"room_id\":\"global\",\"content\":\"ok\"}">>).

json_encode(_) ->
    <<"{\"type\":\"server_response\",\"room_id\":\"global\",\"content\":\"ok\"}">> =
        chatlib_proto:encode({server_response, global, ok}).

proto_encode_decode(_) ->
    Packets = [
        {server_response, global, ok},
        {server_response, 0, room_already_joined},
        {server_response, 0, room_does_not_exist},
        {server_response, 0, room_not_joined},
        get_rooms,
        {join_room, 0},
        {set_name, 0, "Test Name"},
        {send_message, 0, "Test Message"},
        {rooms_notification, global, #{0=>"Test room", 1=>"Best room"}},
        {message_notification, 1, [
            {{{2018, 8, 23}, {10, 26, 22}}, "My Name", "My Message"},
            {{{2018, 8, 23}, {14, 26, 22}}, "Another Name", "My other message"}
        ]}
    ],

    Encoded = lists:map(fun chatlib_proto:encode/1, Packets),
    Decoded = lists:map(fun chatlib_proto:decode/1, Encoded),

    Test = lists:zip(Packets, Decoded),

    lists:foreach(
        fun({P, D}) ->
            P = D
        end,
        Test
    ).