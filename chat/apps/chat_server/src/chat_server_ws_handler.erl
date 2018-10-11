-module(chat_server_ws_handler).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([send/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPE EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export_type([state/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type state() :: [binary()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec send(Message :: library_protocol:message(), Recipient :: pid()) ->
    ok.

send(Message, Recipient) -> % Very bad piece of code
    ok = lager:info("Sending ~p to process ~p", [Message, Recipient]),
    Recipient ! {send, Message},
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(Req :: cowboy_req:req(), Opts :: cowboy_websocket:opts()) ->
    {cowboy_websocket, cowboy_req:req(), cowboy_websocket:opts()}.

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

-spec websocket_init(term()) ->
    {ok, []}.

websocket_init(_) ->
    ok = lager:notice("Initializing websocket, PID: ~p", [self()]),
    {ok, []}.

-spec websocket_handle({text, Json :: jiffy:json_value()}, Subs :: state()) ->
    {ok, term()}.

websocket_handle({text, Json}, Subs) ->
    Message = library_protocol:decode(Json),
    case chat_server_message_handler:handle_message(Message, Subs) of
        {ok, NewSubs} ->
            {ok, NewSubs};
        {error, _} = ErrorMessage ->
            send(ErrorMessage, self()),
            {ok, Subs}
    end;


websocket_handle(_Data, Subs) ->
    {ok, Subs}.

-spec websocket_info
    ({send, library_protocol:message()}, Subs :: state()) ->
        {reply, {text, jiffy:json_value()}, state()};
    ({gproc_ps_event, Room :: binary(), Message :: library_protocol:message()}, Subs :: state()) ->
        {reply, {text, jiffy:json_value()}, state()};
    ({gproc_ps_event, Room :: binary(), room_deleted}, Subs :: state()) ->
        {ok, state()}.

websocket_info({gproc_ps_event, Room, room_deleted}, Subs) ->
    NewSubs = chat_server_message_handler:force_leave(Room, Subs),
    ok = lager:debug("gproc_ps_event, room_deleted"),
    {ok, NewSubs};

websocket_info({gproc_ps_event, _Room, Message}, Subs) ->
    ok = lager:debug("gproc_ps_event, Websocket info: ~p", [Message]),
    Json = library_protocol:encode(Message),
    {reply, {text, Json}, Subs};

websocket_info({send, Message}, Subs) ->
    ok = lager:debug("send, Websocket info: ~p", [Message]),
    Json = library_protocol:encode(Message),
    {reply, {text, Json}, Subs}.

-spec terminate(_Reason :: term(), _Req :: map(), Subs :: state()) ->
    ok.

terminate(_Reason, _Req, _Subs) ->
    ok = lager:info("Websocket process ~p is terminated", [self()]).
