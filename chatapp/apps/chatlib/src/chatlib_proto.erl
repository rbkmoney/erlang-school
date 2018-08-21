-module(chatlib_proto).

-type packet_type() ::
    server_response |
    get_rooms |
    join_room |
    set_name |
    send_message |
    receive_messages.

-type room_id() :: global | non_neg_integer().
-type display_name() :: nonempty_string().
-type message_text() :: nonempty_string().

-type room_message() :: #{
    timestamp => erlang:timestamp(),
    display_name => display_name(),
    message_text => message_text()
}.

-type packet_map() :: #{
    type := packet_type(),
    room_id => room_id(),
    content => display_name() | message_text() | [room_message()]
}.

-type packet_term() ::
    packet_type() |
    {packet_type(), room_id()} |
    {packet_type(), room_id(), display_name() | message_text() | [room_message()]}.

%% API
-export([
    encode/1,
    decode/1
]).


-spec encode(packet_term()) ->
    binary().
encode(Message) ->
    Ejson = packet_term_to_map(Message),
    jiffy:encode(Ejson).

-spec encode(binary()) ->
    packet_term().
decode(Message) ->
    Json = jiffy:decode(Message, [return_maps]),
    packet_map_to_term(Json).


-spec packet_term_to_map(packet_term()) ->
    packet_map().

packet_term_to_map({server_response, ErrorCode}) ->
    #{
        type => server_response,
        content => ErrorCode
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

-spec packet_map_to_term(packet_map()) ->
    packet_term().

packet_map_to_term(Msg = #{<<"type">> := <<"server_response">>}) ->
    #{<<"content">> := ErrorCode} = Msg,

    {server_response, ErrorCode};

packet_map_to_term(#{<<"type">> := <<"get_rooms">>}) ->
    get_rooms;

packet_map_to_term(Msg = #{<<"type">> := <<"join_room">>}) ->
    #{<<"room_id">> := RoomId} = Msg,

    {join_room, decode_room_id(RoomId)};

packet_map_to_term(Msg = #{<<"type">> := <<"set_name">>}) ->
    #{<<"room_id">> := RoomId, <<"content">> := NameString} = Msg,

    {set_name, decode_room_id(RoomId), binary_to_list(NameString)};

packet_map_to_term(Msg = #{<<"type">> := <<"send_message">>}) ->
    #{<<"room_id">> := RoomId, <<"content">> := MessageString} = Msg,

    {send_message, decode_room_id(RoomId), binary_to_list(MessageString)};

packet_map_to_term(Msg = #{<<"type">> := <<"receive_messages">>}) ->
    #{<<"room_id">> := RoomId, <<"content">> := MessageList} = Msg,
    %@todo since server will never receive messages Message list keys will remain binary
    {receive_messages, decode_room_id(RoomId), MessageList}.

-spec decode_room_id(binary() | room_id()) ->
    room_id().
decode_room_id(<<"global">>) ->
    global;
decode_room_id(RoomId) when is_integer(RoomId) ->
    RoomId.

