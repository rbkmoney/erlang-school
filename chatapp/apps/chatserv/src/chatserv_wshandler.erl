-module(chatserv_wshandler).
-behaviour(cowboy_websocket).

%% API
-type rooms_list() :: list(chatlib_proto:room_id()).

-type state() :: #{
    joined_rooms := rooms_list(),
    auth_id := chatlib_proto:auth_id() | unauthorized
}.

-export([
    send_messages/3
]).

%% cowboy_websocket_handler

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

%%
%% API
%%

-spec send_messages(chatlib_proto:auth_id(), chatlib_proto:room_id(), chatlib_proto:message_list()) ->
    ok.
send_messages(MemberId, RoomId, MessageList) ->
    ok = msg_send(MemberId, {message_notification, RoomId, MessageList}).

%%
%% cowboy_websocket_handler
%%
-spec init(cowboy_req:req(), any()) ->
    {cowboy_websocket, cowboy_req:req(), state()}.
init(Req, _State) ->
    {cowboy_websocket, Req, #{joined_rooms => [], auth_id => unauthorized}}.

-spec websocket_init(any()) ->
    {ok, state()}.
websocket_init(State) ->

    UUID = uuid:uuid4(), %@todo an actual auth here, but something like this will do for now

    ok = lager:info("Registered self as  ~p", [{chat_member, UUID}]),
    ok = proc_reg(UUID),

    {ok, State#{auth_id => UUID}}.

-spec websocket_handle({text, binary()}, state()) ->
    {reply, {text, binary()}, state()} | {ok, state()}.
websocket_handle({text, RequestData}, State) ->
    RequestMsg = chatlib_proto:decode(RequestData),

    {ResponseMsg, NewState} = handle_message(RequestMsg, State),

    ResponseData = chatlib_proto:encode(ResponseMsg),
    {reply, {text, ResponseData}, NewState};
websocket_handle(_Data, State) ->
    {ok, State}.

-spec websocket_info({message_notification, chatlib_proto:room_id(), chatlib_proto:message_list()}, state()) ->
    {ok, state()} | {reply, {text, binary()}, state()}.

websocket_info(Msg = {message_notification, _, _}, State = #{auth_id := MemberId}) ->
    Response = chatlib_proto:encode(Msg),
    ok = lager:info("Sending new messages to ~p", [MemberId]),

    {reply, {text, Response}, State};

websocket_info(_Info, State) ->
    {ok, State}.

-spec terminate(_, _, state()) ->
    ok.
terminate(_, _, #{auth_id := MemberId}) ->
    ok = proc_unreg(MemberId),
    ok = lager:info("Unregistered self as  ~p", [{chat_member, MemberId}]).

%%
%% internal
%%
-spec msg_send(chatlib_proto:auth_id(), Msg :: tuple()) ->
    ok.
msg_send(MemberId, Message) ->
    Message = gproc:send(gproc_tuple({chat_member, MemberId}), Message),
    ok.

-spec proc_reg(chatlib_proto:auth_id()) ->
    ok.
proc_reg(MemberId) ->
    true = gproc:reg(gproc_tuple({chat_member, MemberId})),
    ok.

-spec proc_unreg(chatlib_proto:auth_id()) ->
    ok.
proc_unreg(MemberId) ->
    true = gproc:unreg(gproc_tuple({chat_member, MemberId})),
    ok.

-spec gproc_tuple(Id::tuple()) ->
    {n, l, Id::tuple()}.
gproc_tuple(Id) ->
    {n, l, Id}.

-spec handle_message(chatlib_proto:packet(), state()) ->
    {chatlib_proto:packet(), state()}.
handle_message(get_rooms, State) ->
    RoomsList = chatserv_room_manager:get_rooms_with_names(),
    Response = {rooms_notification, global, RoomsList},

    {Response, State};

handle_message({join_room, RoomId}, State = #{joined_rooms := Rooms, auth_id := MemberId}) ->
    case chatserv_room_manager:room_exists(RoomId) of
        true ->
            {UpdatedRooms, Response} = do_join_room(MemberId, RoomId, Rooms),

            {Response, State#{joined_rooms := UpdatedRooms}};

        false ->
            {{server_response, RoomId, room_does_not_exist}, State#{joined_rooms := Rooms}}
    end;

handle_message({set_name, RoomId, NameString}, State = #{joined_rooms := Rooms, auth_id := MemberId}) ->
    Response =
        case room_joined(RoomId, Rooms) of
            true ->
                Result = chatserv_room:change_member_name(MemberId, RoomId, NameString),

                {server_response, RoomId, Result};

            false ->
                {server_response, RoomId, room_not_joined}
        end,

    {Response, State};

handle_message({send_message, RoomId, MessageString}, State = #{joined_rooms := Rooms, auth_id := MemberId}) ->
    Response =
        case room_joined(RoomId, Rooms) of
            true ->
                Result = chatserv_room:send_message(MemberId, RoomId, MessageString),

                {server_response, RoomId, Result};

            false ->
                {server_response, RoomId, room_not_joined}
        end,

    {Response, State}.


-spec do_join_room(chatlib_proto:auth_id(), chatlib_proto:room_id(), rooms_list()) ->
    {rooms_list(), chatlib_proto:packet()}.
do_join_room(MemberId, RoomId, Rooms) ->
    case join_room(MemberId, RoomId, Rooms) of
        {ok, NewRooms} ->
            {NewRooms, {server_response, RoomId, ok}};

        {error, room_already_joined} ->
            {Rooms, {server_response, RoomId, room_already_joined}}
    end.

-spec room_joined(chatlib_proto:room_id(), rooms_list()) ->
    boolean().
room_joined(RoomId, Rooms) ->
    lists:member(RoomId, Rooms).

-spec join_room(chatlib_proto:auth_id(), chatlib_proto:room_id(), rooms_list()) ->
    {error, chatlib_proto:response_code()} | {ok, rooms_list()}.
join_room(MemberId, RoomId, Rooms) ->
    case room_joined(RoomId, Rooms) of
        false ->
            ok = chatserv_room:join(MemberId, RoomId),

            {ok, [RoomId | Rooms]};

        true ->
            {error, room_already_joined}
    end.