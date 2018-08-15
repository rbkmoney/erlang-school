-module(chatlib_sock).

%% API
-export([
    encode/1,
    decode/1
]).

-define(PACKET_ID_get_rooms_list, 1).
-define(PACKET_ID_join_room, 2).
-define(PACKET_ID_set_name, 3).
-define(PACKET_ID_send_message, 4).
-define(PACKET_ID_server_response, 10).
-define(PACKET_ID_receive_messages, 11).


encode(get_rooms_list) ->
    <<?PACKET_ID_get_rooms_list>>;
encode({join_room, RoomId}) ->
    <<?PACKET_ID_join_room, RoomId:16/unsigned>>;
encode({set_name, RoomId, NameString}) ->
    NameBinary = list_to_binary(NameString),
    <<?PACKET_ID_set_name, RoomId:16/unsigned, NameBinary/binary>>;
encode({send_message, RoomId, MessageString}) ->
    MessageBinary = list_to_binary(MessageString),
    <<?PACKET_ID_send_message, RoomId:16/unsigned, MessageBinary/binary>>;
encode({server_response, RoomId, ErrorCode}) ->
    <<?PACKET_ID_server_response, RoomId:16/unsigned, ErrorCode:16/unsigned>>;
encode({receive_messages, RoomId, MessagesList}) ->
    MessagesBin = term_to_binary(MessagesList),
    <<?PACKET_ID_receive_messages, RoomId:16/unsigned, MessagesBin/binary>>).

decode(<<?PACKET_ID_get_rooms_list>>) ->
    get_rooms_list;
decode(<<?PACKET_ID_join_room, RoomId:16/unsigned>>) ->
    {join_room, RoomId};
decode(<<?PACKET_ID_set_name, RoomId:16/unsigned, NameBinary/binary>>) ->
    NameString = binary_to_list(NameBinary),
    {set_name, RoomId, NameString};
decode(<<?PACKET_ID_send_message, RoomId:16/unsigned, MessageBinary/binary>>) ->
    MessageString = binary_to_list(MessageBinary),
    {send_message, RoomId, MessageString};
decode(<<?PACKET_ID_server_response, RoomId:16/unsigned, ErrorCode:16/unsigned>>) ->
    {server_response, RoomId, ErrorCode};
decode(<<?PACKET_ID_receive_messages, RoomId:16/unsigned, MessagesBin/binary>>) ->
    MessagesList = binary_to_term(MessagesBin),
    {receive_messages, RoomId, MessagesList}.