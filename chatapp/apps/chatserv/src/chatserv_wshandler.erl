-module(chatserv_wshandler).
-behaviour(cowboy_websocket).

%% API
-define(DEFAULT_USERNAME, "New User").

-type rooms_map() :: #{chatlib_proto:room_id() => chatlib_proto:member_name()}.

-type state() :: #{
    joined_rooms := rooms_map()
}.

%% cowboy_websocket_handler

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/2
]).

%%
%% API
%%

%%
%% cowboy_websocket_handler
%%
-spec init(cowboy_req:req(), any()) ->
    {cowboy_websocket, cowboy_req:req(), any()}.
init(Req, _State) ->
    {cowboy_websocket, Req, #{joined_rooms => #{}}}.

-spec websocket_init(any()) ->
    {ok, state()}.
websocket_init(State) ->
    {ok, State}.

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

websocket_info(Msg = {message_notification, _, _}, State) ->
    Response = chatlib_proto:encode(Msg),

    {reply, {text, Response}, State};

websocket_info(_Info, State) ->
    {ok, State}.

-spec terminate(_, state()) ->
    ok.
terminate(_Reason, _State) ->
    ok.

%%
%% internal
%%

%remains unchanged
-spec handle_message(chatlib_proto:packet(), state()) ->
    {chatlib_proto:packet(), state()}.
handle_message(get_rooms, State) ->
    RoomsList = chatserv_room_manager:get_rooms_with_names(),
    Response = {rooms_notification, global, RoomsList},

    {Response, State};

%means gproc:reg({p, l, {chat_room, RoomId}})
handle_message({join_room, RoomId}, State = #{joined_rooms := Rooms}) ->
    case chatserv_room_manager:check_room_exists(RoomId) of
        true ->
            {UpdatedRooms, Response} = do_join_room(RoomId, Rooms),

            {Response, State#{joined_rooms := UpdatedRooms}};

        false ->
            {{server_response, RoomId, room_does_not_exist}, State#{joined_rooms := Rooms}}
    end;

%means changing name internally in state
handle_message({set_name, RoomId, NameString}, State = #{joined_rooms := Rooms}) ->
    case do_change_name(RoomId, NameString, Rooms) of
        {ok, NewRooms} ->
            {{server_response, RoomId, ok}, State#{joined_rooms := NewRooms}};

        {error, room_not_joined} ->
            {{server_response, RoomId, room_not_joined}, State}
    end;

%means gproc:msg({p, l, {chat_room, RoomId}}, Message)
handle_message({send_message, RoomId, MessageString}, State = #{joined_rooms := Rooms}) ->
    Response =
        case do_send_message(RoomId, MessageString, Rooms) of
            ok ->
                {server_response, RoomId, ok};

            {error, room_not_joined} ->
                {server_response, RoomId, room_not_joined}
        end,

    {Response, State}.

-spec room_joined(chatlib_proto:room_id(), rooms_map()) ->
    boolean().
room_joined(RoomId, Rooms) ->
    maps:is_key(RoomId, Rooms).

-spec do_join_room(chatlib_proto:room_id(), rooms_map()) ->
    {rooms_map(), chatlib_proto:packet()}.
do_join_room(RoomId, Rooms) ->
    case join_room(RoomId, Rooms) of
        {ok, NewRooms} ->
            {NewRooms, {server_response, RoomId, ok}};

        {error, room_already_joined} ->
            {Rooms, {server_response, RoomId, room_already_joined}}
    end.

-spec join_room(chatlib_proto:room_id(), rooms_map()) ->
    {error, chatlib_proto:response_code()} | {ok, rooms_map()}.
join_room(RoomId, Rooms) ->
    case room_joined(RoomId, Rooms) of
        false ->
            true = gproc:reg({p, l, {chat_room, RoomId}}), %@todo unregister when terminate

            {ok, maps:put(RoomId, ?DEFAULT_USERNAME, Rooms)};

        true ->
            {error, room_already_joined}
    end.

do_change_name(RoomId, NameString, Rooms) ->
    case room_joined(RoomId, Rooms) of
        true ->
            NewRooms = maps:put(RoomId, NameString, Rooms),

            {ok, NewRooms};

        false ->
            {error, room_not_joined}
    end.

do_send_message(RoomId, MessageString, Rooms) ->
    case room_joined(RoomId, Rooms) of
        true ->
            MemberName = maps:get(RoomId, Rooms),
            Message = { erlang:universaltime(), MemberName, MessageString },

            _ = gproc:send({p, l, {chat_room, RoomId}}, {message_notification, RoomId, [Message]}),

            ok;

        false ->
            {error, room_not_joined}
    end.