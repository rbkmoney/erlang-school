-module(chatserv_wshandler).
-behaviour(cowboy_websocket_handler).

%% API
-type state() :: #{
    joined_rooms := #{ chatlib_proto:room_id() => pid() }
}.

-export([
    send_messages_to/3
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

send_messages_to(MemberPid, RoomId, MessageList) ->
    MemberPid ! {receive_messages, RoomId, MessageList},
    ok.

%%
%% cowboy_websocket_handler
%%

%@todo actual types
-spec init({tcp, http}, cowboy_req:req(), any()) ->
    {upgrade, protocol, cowboy_websocket}.
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

%@todo actual types
-spec websocket_init(any(), cowboy_req:req(), any()) ->
    {ok, cowboy_req:req(), state()}.
websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, #{ joined_rooms => #{} }}.

%@todo actual types
-spec websocket_handle({text, binary()}, cowboy_req:req(), state()) ->
    {reply, {text, binary()}, cowboy_req:req(), state()} | {ok, cowboy_req:req(), state()}.
websocket_handle({text, Msg}, Req, State) ->
    Message = chatlib_proto:decode(Msg),
    {Response, NewReq, NewState} = handle_message(Message, Req, State),

    {reply, {text, Response}, NewReq, NewState};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

%@todo actual types
-spec websocket_info(any(), cowboy_req:req(), state()) ->
    {ok, cowboy_req:req(), state()} | {reply, {text, binary()}, cowboy_req:req(), state()}.

websocket_info({receive_messages, RoomId, MessageList}, Req, State) ->
    PreparedMessages = lists:map(
        fun(M = #{timestamp := Timestamp, member_name := Name, message_text := Text}) ->
            M#{
                timestamp => calendar:datetime_to_gregorian_seconds(Timestamp),
                member_name => list_to_binary(Name),
                message_text => list_to_binary(Text)
            }
        end,
        MessageList
    ),
    Response = chatlib_proto:encode({receive_messages, RoomId, PreparedMessages}),

    {reply, {text, Response}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

%@todo actual types
-spec websocket_terminate(any(), cowboy_req:req(), any()) ->
    ok.
websocket_terminate(_Reason, _Req, _State) ->
    ok.

%%
%% internal
%%


%@todo actual types
-spec handle_message(chatlib_proto:packet_term(), cowboy_req:req(), state()) ->
    {binary(), cowboy_req:req(), state()}.
handle_message(get_rooms, Req, State) ->
    RoomsList = chatserv_room_manager:get_rooms_list(),
    PreparedList = maps:fold(
        fun(Id, Name, Acc) ->
            Acc ++ [#{
                room_id => Id,
                room_name => list_to_binary(Name)
            }]
        end,
        [], RoomsList
    ),

    Response = chatlib_proto:encode({server_response, global, PreparedList}),
    {Response, Req, State};

handle_message({join_room, RoomId}, Req, State = #{ joined_rooms := Rooms }) ->
    RoomPid = chatserv_room_manager:get_room(RoomId),

    case chatserv_room:join_to(RoomPid, self()) of
        ok ->
            NewRooms = maps:put(RoomId, RoomPid, Rooms),
            Code = ok;
        badarg ->
            NewRooms = Rooms,
            Code = user_already_exists
    end,

    Response = chatlib_proto:encode({server_response, RoomId, Code}),
    {Response, Req, State#{ joined_rooms := NewRooms }};

handle_message({set_name, RoomId, NameString}, Req, State = #{ joined_rooms := Rooms }) ->
    RoomPid = maps:get(RoomId, Rooms),
    Resp = chatserv_room:change_name_in(RoomPid, self(), NameString),

    Response = chatlib_proto:encode({server_response, RoomId, Resp}),
    {Response, Req, State};

handle_message({send_message, RoomId, MessageString}, Req, State = #{ joined_rooms := Rooms }) ->
    RoomPid = maps:get(RoomId, Rooms),
    Resp = chatserv_room:send_message_to(RoomPid, self(), MessageString),

    Response = chatlib_proto:encode({server_response, RoomId, Resp}),
    {Response, Req, State}.