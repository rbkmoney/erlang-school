-module(chatlib_proto).

%% API
-type packet_type() ::
    server_response |
    get_rooms |
    join_room |
    set_name |
    send_message |
    receive_rooms |
    receive_messages.

-type member_name() :: nonempty_string().
-type message_text() :: nonempty_string().

-type room_id() :: global | non_neg_integer().
-type room_name() :: nonempty_string().

-type room_list() :: #{ room_id() => room_name() }.

-type room_message() :: { erlang:timestamp(), member_name(), message_text() }.
-type message_list() :: list(room_message()).

-type response_code() ::
    ok |
    room_already_joined |
    room_does_not_exist |
    room_not_joined.

-type packet_content() ::
    response_code() |
    member_name() |
    message_text() |
    message_list() |
    room_list().

-type packet() ::
    packet_type() |
    {packet_type(), room_id()} |
    {packet_type(), room_id(), packet_content()}.

-export_type([
    response_code/0,
    room_id/0,
    room_name/0,
    room_list/0,
    room_message/0,
    member_name/0,
    message_text/0,
    message_list/0,
    packet/0
]).

-export([
    encode/1,
    decode/1,
    test/0
]).

%%
%% API
%%

-spec encode(packet()) ->
    binary().
encode(Message) ->
    Ejson = packet_to_json(Message),
    jiffy:encode(Ejson).

-spec decode(binary()) ->
    packet().
decode(Message) ->
    Json = jiffy:decode(Message, [return_maps]),
    packet_from_json(Json).

test() ->
    Packets = [
        {server_response, global, ok},
        {server_response, 0, room_already_joined},
        {server_response, 0, room_does_not_exist},
        {server_response, 0, room_not_joined},
        get_rooms,
        {join_room, 0},
        {set_name, 0, "Test Name"},
        {send_message, 0, "Test Message"},
        {receive_rooms, global, #{0=>"Test room", 1=>"Best room"}},
        {receive_messages, 1, [
            {{{2018, 8, 23}, {10, 26, 22}}, "My Name", "My Message"},
            {{{2018, 8, 23}, {14, 26, 22}}, "Another Name", "My other message"}
        ]}
    ],

    Encoded = lists:map(
        fun(Packet) ->
            encode(Packet)
        end,
        Packets
    ),

    Decoded = lists:map(
        fun(Packet) ->
            decode(Packet)
        end,
        Encoded
    ),

    Test = lists:zip(Packets, Decoded),

    lists:foreach(
        fun({P, D}) ->
            P = D
        end,
        Test
    ),

    ok.
%%
%% Internal
%%
-type json() :: #{
    type := packet_type(),
    room_id => room_id(),
    content => json_content()
}.

-type json_content() ::
    response_code() |
    member_name() |
    message_text() |
    message_list_json() |
    room_list_json().

-type room_json() :: #{
    room_id => non_neg_integer(),
    room_name => binary()
}.
-type room_list_json() :: list(room_json()).

-type message_json() :: #{
    timestamp => non_neg_integer(),
    member_name => binary(),
    message_text => binary()
}.
-type message_list_json() :: list(message_json()).

%% Encoders
-spec packet_to_json(packet()) ->
    json().
packet_to_json({server_response, RoomId, Message}) ->
    #{
        type => server_response,
        room_id => RoomId,
        content => Message
    };

packet_to_json(get_rooms) ->
    #{
        type => get_rooms
    };

packet_to_json({join_room, RoomId}) ->
    #{
        type => join_room,
        room_id => RoomId
    };

packet_to_json({set_name, RoomId, NameString}) ->
    #{
        type => set_name,
        room_id => RoomId,
        content => list_to_binary(NameString)
    };

packet_to_json({send_message, RoomId, MessageString}) ->
    #{
        type => send_message,
        room_id => RoomId,
        content => list_to_binary(MessageString)
    };

packet_to_json({receive_rooms, global, MessageList}) ->
    #{
        type => receive_rooms,
        room_id => global,
        content => room_list_to_json(MessageList)
    };

packet_to_json({receive_messages, RoomId, MessageList}) ->
    #{
        type => receive_messages,
        room_id => RoomId,
        content => message_list_to_json(MessageList)
    }.

-spec room_list_to_json(room_list()) ->
    room_list_json().
room_list_to_json(RoomsList) ->
    maps:fold(
        fun(Id, Name, Acc) ->
            Acc ++ [#{
                room_id => Id,
                room_name => list_to_binary(Name)
            }]
        end,
        [], RoomsList
    ).

-spec message_list_to_json(message_list()) ->
    message_list_json().
message_list_to_json(MessageList) ->
    lists:map(
        fun({Timestamp, Name, Text}) ->
            #{
                timestamp => calendar:datetime_to_gregorian_seconds(Timestamp),
                member_name => list_to_binary(Name),
                message_text => list_to_binary(Text)
            }
        end,
        MessageList
    ).

%% Decoders
-spec packet_from_json(json()) ->
    packet().
packet_from_json(Msg = #{<<"type">> := <<"server_response">>}) ->
    #{<<"room_id">> := RoomId, <<"content">> := ResponseCode} = Msg,

    {server_response, decode_room_id(RoomId), decode_response_code(ResponseCode)};

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

packet_from_json(Msg = #{<<"type">> := <<"receive_rooms">>}) ->
    #{<<"room_id">> := RoomId, <<"content">> := RoomListJson} = Msg,

    {receive_rooms, decode_room_id(RoomId), decode_room_list(RoomListJson)};

packet_from_json(Msg = #{<<"type">> := <<"receive_messages">>}) ->
    #{<<"room_id">> := RoomId, <<"content">> := MessageListJson} = Msg,

    {receive_messages, decode_room_id(RoomId), decode_message_list(MessageListJson)}.

-spec decode_room_id(binary() | room_id()) ->
    room_id().
decode_room_id(<<"global">>) ->
    global;
decode_room_id(RoomId) when is_integer(RoomId) ->
    RoomId;
decode_room_id(_) ->
    throw(badarg).

-spec decode_response_code(binary()) ->
    response_code().
decode_response_code(<<"ok">>) ->
    ok;
decode_response_code(<<"room_does_not_exist">>) ->
    room_does_not_exist;
decode_response_code(<<"room_already_joined">>) ->
    room_already_joined;
decode_response_code(<<"room_not_joined">>) ->
    room_not_joined;
decode_response_code(_) ->
    throw(badarg).

-type binary_room_list() :: list(#{binary() => non_neg_integer() | binary()}).

-spec decode_room_list(binary_room_list()) ->
    room_list().
decode_room_list(RoomsList) ->
    lists:foldl(
        fun(Room, Acc) ->
            #{
                <<"room_id">> := Id,
                <<"room_name">> := Name
            } = Room,

            maps:put(Id, binary_to_list(Name), Acc)
        end,
        #{}, RoomsList
    ).

-type binary_message_list() :: list(#{binary() => non_neg_integer() | binary()}).

-spec decode_message_list(binary_message_list()) ->
    message_list().
decode_message_list(MessageList) ->
    lists:map(
        fun(Message) ->
            #{
                <<"timestamp">> := Timestamp,
                <<"member_name">> := Name,
                <<"message_text">> := Text
            } = Message,

            {calendar:gregorian_seconds_to_datetime(Timestamp), binary_to_list(Name), binary_to_list(Text)}
        end,
        MessageList
    ).
