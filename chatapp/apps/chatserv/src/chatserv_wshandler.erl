-module(chatserv_wshandler).
-behaviour(cowboy_websocket_handler).

-export([
    init/3,
    websocket_init/3,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3
]).

%%
%% cowboy_websocket_handler
%%

%@todo actual types
-spec init({tcp, http}, any(), any()) ->
    {upgrade, protocol, cowboy_websocket}.
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

%@todo actual types
-spec websocket_init(any(), any(), any()) ->
    {ok, any(), undefined_state}.
websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, undefined_state}.

%@todo actual types
-spec websocket_handle(any(), any(), any()) ->
    tuple().
websocket_handle({text, Msg}, Req, State) ->
    Message = chatlib_proto:decode(Msg),
    {Response, NewReq, NewState} = handle_message(Message, Req, State),

    {reply, {text, Response}, NewReq, NewState};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

%@todo actual types
-spec websocket_info(any(), any(), any()) ->
    {ok, any(), any()}.
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

%@todo actual types
-spec websocket_terminate(any(), any(), any()) ->
    ok.
websocket_terminate(_Reason, _Req, _State) ->
    ok.

%%
%% internal
%%


%@todo actual types
-spec handle_message(chatlib_proto:packet_term(), any(), any()) ->
    {binary(), any(), any()}.
handle_message(get_rooms, Req, State) ->
    Response = chatlib_proto:encode({server_response, 1}),
    {Response, Req, State};

handle_message({join_room, _RoomId}, Req, State) ->
    Response = chatlib_proto:encode({server_response, 2}),
    {Response, Req, State};

handle_message({set_name, _RoomId, _NameString}, Req, State) ->
    Response = chatlib_proto:encode({server_response, 3}),
    {Response, Req, State};

handle_message({send_message, _RoomId, _MessageString}, Req, State) ->
    Response = chatlib_proto:encode({server_response, 4}),
    {Response, Req, State}.