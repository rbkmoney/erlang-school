-module(chat_server_ws_handler).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([init            /2]).
-export([terminate       /3]).
-export([websocket_init  /1]).
-export([websocket_info  /2]).
-export([websocket_handle/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPE EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export_type([state/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type state() :: chat_server_message_handler:subscribers().

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
            {reply, {text, library_protocol:encode(ErrorMessage)}, Subs} % Not sure if it's OK
    end;

websocket_handle(_, Subs) ->
    {ok, Subs}.

-spec websocket_info(term(), Subs :: state()) ->
    {reply, {text, jiffy:json_value()}, state()}.

websocket_info(Event, Subs) ->
    ok = lager:debug("ws_handler caught an event ~p", [Event]),
    {EventBody, NewSubs} = chat_server_message_handler:handle_event(Event, Subs),
    Json = library_protocol:encode(EventBody),
    {reply, {text, Json}, NewSubs}.

-spec terminate(_Reason :: term(), _Req :: map(), Subs :: state()) ->
    ok.

terminate(_Reason, _Req, _Subs) ->
    ok = lager:info("Websocket process ~p is terminated", [self()]).
