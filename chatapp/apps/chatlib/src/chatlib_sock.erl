-module(chatlib_sock).

%% API

-type room_id() :: non_neg_integer().

-type packet() ::
    get_rooms_list |
    {join_room, RoomId :: room_id()} |
    {set_name, RoomId :: room_id(), Name :: nonempty_string()} |
    {send_message, RoomId :: room_id(), Message :: nonempty_string()} |
    {server_response, Message :: term()} |
    {receive_messages,
        RoomId :: room_id(),
        [{Time :: erlang:timestamp(), Name :: nonempty_string(), Message :: nonempty_string()}]
    }.

-export_type([
    packet/0
]).

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


%%
%% IMPORTANT NOTICE:
%% At the time of initially writing this I was not aware that term_to_binary supported atoms (for some reason it
%% badarg'd and I assumed it was that). So for now just assume that this protocol is a way of saving bandwidth
%% (atoms are somewhat big in byte form).
%%


%%
%% Encoders
%%

-spec encode(packet()) ->
    binary().

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

encode({server_response, MessageTerm}) ->
    MessageBinary = term_to_binary(MessageTerm),
    <<?PACKET_ID_server_response, MessageBinary/binary>>;

encode({receive_messages, RoomId, MessagesList}) ->
    MessagesBin = term_to_binary(MessagesList),
    <<?PACKET_ID_receive_messages, RoomId:16/unsigned, MessagesBin/binary>>.

%%
%% Decoders
%%

-spec decode(binary()) ->
    packet().

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
decode(<<?PACKET_ID_server_response, MessageBinary/binary>>) ->
    MessageTerm = binary_to_term(MessageBinary),
    {server_response, MessageTerm};
decode(<<?PACKET_ID_receive_messages, RoomId:16/unsigned, MessagesBin/binary>>) ->
    MessagesList = binary_to_term(MessagesBin),
    {receive_messages, RoomId, MessagesList}.