-module(chatlib_proto).

%% API
-type packet_type() ::
    server_response |
    get_rooms |
    join_room |
    set_name |
    send_message |
    message_notification |
    rooms_notification.

-type member_name() :: nonempty_string().
-type message_text() :: nonempty_string().

-type room_id_direct() :: non_neg_integer().
-type room_id() :: global | room_id_direct().
-type room_name() :: nonempty_string().

-type room_list() :: #{room_id_direct() => room_name()}.

-type room_message() :: {calendar:datetime(), member_name(), message_text()}.
-type message_list() :: list(room_message()).

-type response_code() ::
    ok |
    room_already_joined |
    room_does_not_exist |
    room_not_joined.

-type packet() ::
    get_rooms |
    {join_room, room_id_direct()} |
    {set_name, room_id_direct(), member_name()} |
    {send_message, room_id_direct(), message_text()} |
    {rooms_notification, global, room_list()} |
    {message_notification, room_id_direct(), message_list()} |
    {server_response, room_id(), response_code()}.

-export_type([
    response_code/0,
    room_id/0,
    room_id_direct/0,
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
    decode/1
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
%%
%% Internal
%%
-type json() :: jiffy:json_value().

-type room_json() :: jiffy:json_value().
-type room_list_json() :: list(room_json()).

-type message_json() :: jiffy:json_value().
-type message_list_json() :: list(message_json()).

%% Encoders
-spec packet_to_json(packet()) ->
    json().
packet_to_json({server_response, RoomId, Message}) ->
    make_json(server_response, RoomId, Message);

packet_to_json(get_rooms) ->
    make_json(get_rooms);

packet_to_json({join_room, RoomId}) ->
    make_json(join_room, RoomId);

packet_to_json({set_name, RoomId, NameString}) ->
    make_json(set_name, RoomId, list_to_binary(NameString));

packet_to_json({send_message, RoomId, MessageString}) ->
    make_json(send_message, RoomId, list_to_binary(MessageString));

packet_to_json({rooms_notification, global, RoomList}) ->
    make_json(rooms_notification, global, room_list_to_json(RoomList));

packet_to_json({message_notification, RoomId, MessageList}) ->
    make_json(message_notification, RoomId, message_list_to_json(MessageList)).

-spec make_json(packet_type()) -> json().
make_json(Type) ->
    maps:put(<<"type">>, encode_packet_type(Type), #{}).

-spec make_json(packet_type(), room_id()) ->
    json().
make_json(Type, RoomId) ->
    maps:put(<<"room_id">>, encode_room_id(RoomId), make_json(Type)).

-spec make_json(packet_type(), room_id(), json()) -> json().
make_json(Type, RoomId, Content) ->
    maps:put(<<"content">>, Content, make_json(Type, RoomId)).

-spec room_to_json(room_id_direct(), room_name()) ->
    room_json().
room_to_json(Id, Name) ->
    #{
        <<"room_id">> => Id,
        <<"room_name">> => list_to_binary(Name)
    }.

-spec room_list_to_json(room_list()) ->
    room_list_json().
room_list_to_json(RoomsList) ->
    maps:fold(
        fun(Id, Name, Acc) ->
            Acc ++ [room_to_json(Id, Name)]
        end,
        [], RoomsList
    ).

-spec message_to_json(room_message()) ->
    message_json().
message_to_json({Timestamp, Name, Text}) ->
    #{
        <<"timestamp">> => calendar:datetime_to_gregorian_seconds(Timestamp),
        <<"member_name">> => list_to_binary(Name),
        <<"message_text">> => list_to_binary(Text)
    }.

-spec message_list_to_json(message_list()) ->
    message_list_json().
message_list_to_json(MessageList) ->
    lists:map(fun message_to_json/1, MessageList).

%% Decoders
-spec packet_from_json(json()) ->
    packet().
packet_from_json(Msg = #{<<"type">> := <<"server_response">>}) ->
    {RoomId, ResponseCode} = parse_json(Msg),

    {server_response, decode_room_id(RoomId), decode_response_code(ResponseCode)};

packet_from_json(#{<<"type">> := <<"get_rooms">>}) ->
    get_rooms;

packet_from_json(Msg = #{<<"type">> := <<"join_room">>}) ->
    {RoomId} = parse_json(Msg),

    {join_room, decode_room_id(RoomId)};

packet_from_json(Msg = #{<<"type">> := <<"set_name">>}) ->
    {RoomId, NameString} = parse_json(Msg),

    {set_name, decode_room_id(RoomId), binary_to_list(NameString)};

packet_from_json(Msg = #{<<"type">> := <<"send_message">>}) ->
    {RoomId, MessageString} = parse_json(Msg),

    {send_message, decode_room_id(RoomId), binary_to_list(MessageString)};

packet_from_json(Msg = #{<<"type">> := <<"rooms_notification">>}) ->
    {RoomId, RoomListJson} = parse_json(Msg),

    {rooms_notification, decode_room_id(RoomId), decode_room_list(RoomListJson)};

packet_from_json(Msg = #{<<"type">> := <<"message_notification">>}) ->
    {RoomId, MessageListJson} = parse_json(Msg),

    {message_notification, decode_room_id(RoomId), decode_message_list(MessageListJson)}.

-spec parse_json(json()) ->
    {json(), json()} | {json()}.
parse_json(#{<<"room_id">> := RoomId, <<"content">> := Content}) ->
    {RoomId, Content};
parse_json(#{<<"room_id">> := RoomId}) ->
    {RoomId}.

-spec encode_packet_type(packet_type()) ->
    binary().
encode_packet_type(Type) ->
    list_to_binary(atom_to_list(Type)).

-spec encode_room_id(room_id()) ->
    binary() | non_neg_integer().
encode_room_id(global) ->
    <<"global">>;
encode_room_id(RoomId) when is_integer(RoomId) ->
    RoomId;
encode_room_id(_) ->
    throw(badarg).

-spec decode_room_id(binary() | room_id_direct()) ->
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

-spec decode_room(room_json()) ->
    {room_id_direct(), room_name()}.
decode_room(#{<<"room_id">> := Id, <<"room_name">> := Name}) ->
    {Id, binary_to_list(Name)}.

-type binary_room_list() :: list(#{binary() => non_neg_integer() | binary()}).

-spec decode_room_list(binary_room_list()) ->
    room_list().
decode_room_list(RoomsList) ->
    lists:foldl(
        fun(Room, Acc) ->
            {Id, Name} = decode_room(Room),

            maps:put(Id, Name, Acc)
        end,
        #{}, RoomsList
    ).

-spec decode_message(message_json()) ->
    room_message().
decode_message(#{<<"timestamp">> := Timestamp, <<"member_name">> := Name, <<"message_text">> := Text}) ->
    {calendar:gregorian_seconds_to_datetime(Timestamp), binary_to_list(Name), binary_to_list(Text)}.

-type binary_message_list() :: list(#{binary() => non_neg_integer() | binary()}).

-spec decode_message_list(binary_message_list()) ->
    message_list().
decode_message_list(MessageList) ->
    lists:map(fun decode_message/1, MessageList).
