-module(chatlib_proto).

%% API
-type packet_type() ::
    server_response |
    get_rooms |
    join_room |
    set_name |
    send_message |
    receive_messages.

-type room_id() :: global | non_neg_integer().
-type room_name() :: nonempty_string().

-type member_name() :: nonempty_string().
-type message_text() :: nonempty_string().

-type req_status() ::
    ok |
    user_already_exists |
    room_not_joined.

-type member_message() :: #{
    timestamp => erlang:timestamp(),
    member_name => member_name(),
    message_text => message_text()
}.

-type packet() ::
    packet_type() |
    {packet_type(), room_id()} |
    {packet_type(), room_id(), req_status() | member_name() | message_text() | [member_message()]}.

-export_type([
    room_id/0,
    room_name/0,
    message_text/0,
    member_message/0,
    member_name/0,
    packet/0,
    req_status/0
]).

-export([
    encode/1,
    decode/1
]).

%%
%% API
%%

-spec encode(packet()) ->
    binary().
encode(Message) ->
    Ejson = packet_term_to_map(Message),
    jiffy:encode(Ejson).

-spec decode(binary()) ->
    packet().
decode(Message) ->
    Json = jiffy:decode(Message, [return_maps]),
    packet_from_json(Json).


%% local

-type json() :: #{
    type := packet_type(),
    room_id => room_id(),
    content => req_status() | member_name() | message_text() | [member_message()]
}.

-spec packet_term_to_map(packet()) ->
    json().

packet_term_to_map({server_response, RoomId, Message}) ->
    #{
        type => server_response,
        room_id => RoomId,
        content => Message
    };

packet_term_to_map(get_rooms) ->
    #{
        type => get_rooms
    };

packet_term_to_map({join_room, RoomId}) ->
    #{
        type => join_room,
        room_id => RoomId
    };

packet_term_to_map({set_name, RoomId, NameString}) ->
    #{
        type => set_name,
        room_id => RoomId,
        content => list_to_binary(NameString)
    };

packet_term_to_map({send_message, RoomId, MessageString}) ->
    #{
        type => send_message,
        room_id => RoomId,
        content => list_to_binary(MessageString)
    };

packet_term_to_map({receive_messages, RoomId, MessageList}) ->
    #{
        type => receive_messages,
        room_id => RoomId,
        content => MessageList
    }.

-spec packet_from_json(json()) ->
    packet().

packet_from_json(Msg = #{<<"type">> := <<"server_response">>}) ->
    #{<<"content">> := ErrorCode} = Msg,

    {server_response, ErrorCode};

packet_from_json(#{<<"type">> := <<"get_rooms">>}) ->
    get_rooms;

packet_from_json(Msg = #{<<"type">> := <<"join_room">>}) ->
    #{<<"room_id">> := RoomId} = Msg,

    {join_room, decode_room_id(RoomId)};

packet_from_json(Msg = #{<<"type">> := <<"set_name">>}) ->
    #{<<"room_id">> := RoomId, <<"content">> := NameString} = Msg,

    {set_name, decode_room_id(RoomId), binary_to_list(NameString)};

packet_from_json(Msg = #{<<"type">> := <<"send_message">>}) ->
    #{<<"room_id">> := RoomId, <<"content">> := MessageString} = Msg,

    {send_message, decode_room_id(RoomId), binary_to_list(MessageString)};

packet_from_json(Msg = #{<<"type">> := <<"receive_messages">>}) ->
    #{<<"room_id">> := RoomId, <<"content">> := MessageList} = Msg,
    %since server will never receive messages Message list keys will remain binary
    {receive_messages, decode_room_id(RoomId), MessageList}.

-spec decode_room_id(binary() | room_id()) ->
    room_id().
decode_room_id(<<"global">>) ->
    global;
decode_room_id(RoomId) when is_integer(RoomId) ->
    RoomId.
