-module(chatserv_wshandler).
-behaviour(cowboy_websocket_handler).

%% API
-type state() :: #{
    joined_rooms := #{ chatlib_proto:room_id() => pid() }
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
    {ok, Req, #{ joined_rooms => #{} }}.

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

websocket_info({receive_messages, RoomId, MessageList}, Req, State) ->
    Response = chatlib_proto:encode({receive_messages, RoomId, MessageList}),

    {reply, {text, Response}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

-spec websocket_terminate({normal, shutdown | timeout} |
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

handle_message({join_room, RoomId}, Req, State = #{ joined_rooms := Rooms }) ->
    RoomPid = chatserv_room_manager:get_room_pid(RoomId),

    Code = case chatserv_room:join(RoomPid) of
        ok ->
            NewRooms = maps:put(RoomId, RoomPid, Rooms),
            ok;
        user_already_exists ->
            NewRooms = Rooms,
            user_already_exists
    end,

    Response = chatlib_proto:encode({server_response, RoomId, Code}),
    {Response, Req, State#{ joined_rooms := NewRooms }};

handle_message({set_name, RoomId, NameString}, Req, State = #{ joined_rooms := Rooms }) ->
    case maps:is_key(RoomId, Rooms) of
       true ->
           RoomPid = maps:get(RoomId, Rooms),
           Resp = chatserv_room:change_name(RoomPid, NameString),

           Response = chatlib_proto:encode({server_response, RoomId, Resp});
       false ->
           Response = chatlib_proto:encode({server_response, RoomId, room_not_joined})
    end,

    {Response, Req, State};

handle_message({send_message, RoomId, MessageString}, Req, State = #{ joined_rooms := Rooms }) ->
    case maps:is_key(RoomId, Rooms) of
        true ->
            RoomPid = maps:get(RoomId, Rooms),
            Resp = chatserv_room:send_message(RoomPid, MessageString),

            Response = chatlib_proto:encode({server_response, RoomId, Resp});
        false ->
            Response = chatlib_proto:encode({server_response, RoomId, room_not_joined})
    end,

    {Response, Req, State}.
