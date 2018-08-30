-module(chatserv_wshandler).
-behaviour(cowboy_websocket).

%% API
-type room_pid() :: pid().
-type rooms_map() :: #{chatlib_proto:room_id() => room_pid()}.

-type state() :: #{
    joined_rooms := rooms_map()
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
    websocket_terminate/2
]).

%%
%% API
%%

-spec send_messages(pid(), chatlib_proto:room_id(), chatlib_proto:message_list()) ->
    ok.
send_messages(MemberPid, RoomId, MessageList) ->
    MemberPid ! {message_notification, RoomId, MessageList},
    ok.

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

-spec websocket_terminate(_, state()) ->
    ok.
websocket_terminate(_Reason, _State) ->
    ok.

%%
%% internal
%%
-spec handle_message(chatlib_proto:packet(), state()) ->
    {chatlib_proto:packet(), state()}.
handle_message(get_rooms, State) ->
    RoomsList = chatserv_room_manager:get_rooms_with_names(),
    Response = {rooms_notification, global, RoomsList},

    {Response, State};

handle_message({join_room, RoomId}, State = #{joined_rooms := Rooms}) ->
    case chatserv_room_manager:get_room_pid(RoomId) of
        {ok, Pid} ->
            {UpdatedRooms, Response} = do_join_room(Pid, RoomId, Rooms),

            {Response, State#{joined_rooms := UpdatedRooms}};

        {error, room_does_not_exist} ->
            {{server_response, RoomId, room_does_not_exist}, State#{joined_rooms := Rooms}}
    end;

handle_message({set_name, RoomId, NameString}, State = #{joined_rooms := Rooms}) ->
    Response =
        case get_room_pid(RoomId, Rooms) of
            {ok, RoomPid} ->
                Result = chatserv_room:change_member_name(RoomPid, NameString),

                {server_response, RoomId, Result};

            {error, room_not_joined} ->
                {server_response, RoomId, room_not_joined}
        end,

    {Response, State};

handle_message({send_message, RoomId, MessageString}, State = #{joined_rooms := Rooms}) ->
    Response =
        case get_room_pid(RoomId, Rooms) of
            {ok, RoomPid} ->
                Result = chatserv_room:send_message(RoomPid, MessageString),

                {server_response, RoomId, Result};

            {error, room_not_joined} ->
                {server_response, RoomId, room_not_joined}
        end,

    {Response, State}.


-spec do_join_room(room_pid(), chatlib_proto:room_id(), rooms_map()) ->
    {rooms_map(), chatlib_proto:packet()}.
do_join_room(Pid, RoomId, Rooms) ->
    case join_room(Pid, RoomId, Rooms) of
        {ok, NewRooms} ->
            {NewRooms, {server_response, RoomId, ok}};

        {error, room_already_joined} ->
            {Rooms, {server_response, RoomId, room_already_joined}}
    end.

-spec room_joined(chatlib_proto:room_id(), rooms_map()) ->
    boolean().
room_joined(RoomId, Rooms) ->
    maps:is_key(RoomId, Rooms).

-spec join_room(room_pid(), chatlib_proto:room_id(), rooms_map()) ->
    {error, chatlib_proto:response_code()} | {ok, rooms_map()}.
join_room(RoomPid, RoomId, Rooms) ->
    case room_joined(RoomId, Rooms) of
        false ->
            ok = chatserv_room:join(RoomPid),

            {ok, maps:put(RoomId, RoomPid, Rooms)};

        true ->
            {error, room_already_joined}
    end.

-spec get_room_pid(chatlib_proto:room_id(), rooms_map()) ->
    {error, chatlib_proto:response_code()} | {ok, room_pid()}.
get_room_pid(RoomId, Rooms) ->
    case room_joined(RoomId, Rooms) of
        true ->
            {ok, maps:get(RoomId, Rooms)};

        false ->
            {error, room_not_joined}
    end.