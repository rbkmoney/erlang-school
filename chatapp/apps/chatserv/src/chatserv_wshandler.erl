-module(chatserv_wshandler).
-behaviour(cowboy_websocket_handler).

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
    init/3,
    websocket_init/3,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3
]).

%%
%% API
%%

-spec send_messages(pid(), chatlib_proto:room_id(), chatlib_proto:message_list()) ->
    ok.
send_messages(MemberPid, RoomId, MessageList) ->
    MemberPid ! {receive_messages, RoomId, MessageList},
    ok.

%%
%% cowboy_websocket_handler
%%
-spec init({tcp, http}, cowboy_req:req(), any()) ->
    {upgrade, protocol, cowboy_websocket}.
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

-spec websocket_init(tcp | ssl, cowboy_req:req(), any()) ->
    {ok, cowboy_req:req(), state()}.
websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, #{joined_rooms => #{}}}.

-spec websocket_handle({text, binary()}, cowboy_req:req(), state()) ->
    {reply, {text, binary()}, cowboy_req:req(), state()} | {ok, cowboy_req:req(), state()}.
websocket_handle({text, Msg}, Req, State) ->
    Message = chatlib_proto:decode(Msg),
    {Response, NewReq, NewState} = handle_message(Message, Req, State),

    {reply, {text, Response}, NewReq, NewState};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

-spec websocket_info(
    {receive_messages, chatlib_proto:room_id(), chatlib_proto:message_list()},
    cowboy_req:req(), state()
) ->
    {ok, cowboy_req:req(), state()} | {reply, {text, binary()}, cowboy_req:req(), state()}.

websocket_info(Msg = {receive_messages, _, _}, Req, State) ->
    Response = chatlib_proto:encode(Msg),

    {reply, {text, Response}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

-spec websocket_terminate(
    {normal, shutdown | timeout} |
    {remote, closed} |
    {remote, cowboy_websocket:close_code(), binary()} |
    {error, badencoding | badframe | closed | atom()},
    cowboy_req:req(), state()
) ->
    ok.
websocket_terminate(_Reason, _Req, _State) ->
    ok.

%%
%% internal
%%
-spec handle_message(chatlib_proto:packet(), cowboy_req:req(), state()) ->
    {binary(), cowboy_req:req(), state()}.
handle_message(get_rooms, Req, State) ->
    RoomsList = chatserv_room_manager:get_rooms_with_names(),
    Response = chatlib_proto:encode({receive_rooms, global, RoomsList}),

    {Response, Req, State};

handle_message({join_room, RoomId}, Req, State = #{joined_rooms := Rooms}) ->
    case chatserv_room_manager:get_room_pid(RoomId) of
        {error, room_does_not_exist} ->
            UpdatedRooms = Rooms,
            Response = chatlib_proto:encode({server_response, RoomId, room_does_not_exist});

        {ok, Pid} ->
            {UpdatedRooms, Response} = do_join_room(Pid, RoomId, Rooms)
    end,

    {Response, Req, State#{joined_rooms := UpdatedRooms}};

handle_message({set_name, RoomId, NameString}, Req, State = #{joined_rooms := Rooms}) ->
    case get_room_pid(RoomId, Rooms) of
        {error, room_not_joined} ->
            Response = chatlib_proto:encode({server_response, RoomId, room_not_joined});

        {ok, RoomPid} ->
            Result = chatserv_room:change_name(RoomPid, NameString),

            Response = chatlib_proto:encode({server_response, RoomId, Result})
    end,

    {Response, Req, State};

handle_message({send_message, RoomId, MessageString}, Req, State = #{joined_rooms := Rooms}) ->
    case get_room_pid(RoomId, Rooms) of
        {error, room_not_joined} ->
            Response = chatlib_proto:encode({server_response, RoomId, room_not_joined});

        {ok, RoomPid} ->
            Result = chatserv_room:send_message(RoomPid, MessageString),

            Response = chatlib_proto:encode({server_response, RoomId, Result})
    end,

    {Response, Req, State}.


-spec do_join_room(room_pid(), chatlib_proto:room_id(), rooms_map()) ->
    {rooms_map(), binary()}.
do_join_room(Pid, RoomId, Rooms) ->
    case join_room(Pid, RoomId, Rooms) of
        {error, room_already_joined} ->
            UpdatedRooms = Rooms,

            Response = chatlib_proto:encode({server_response, RoomId, room_already_joined});

        {ok, NewRooms} ->
            UpdatedRooms = NewRooms,

            Response = chatlib_proto:encode({server_response, RoomId, ok})
    end,

    {UpdatedRooms, Response}.

-spec room_joined(chatlib_proto:room_id(), rooms_map()) ->
    boolean().
room_joined(RoomId, Rooms) ->
    maps:is_key(RoomId, Rooms).

-spec join_room(room_pid(), chatlib_proto:room_id(), rooms_map()) ->
    {error, chatlib_proto:response_code()} | {ok, rooms_map()}.
join_room(RoomPid, RoomId, Rooms) ->
    case room_joined(RoomId, Rooms) of
        true ->
            {error, room_already_joined};

        false ->
            ok = chatserv_room:join(RoomPid),

            {ok, maps:put(RoomId, RoomPid, Rooms)}
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