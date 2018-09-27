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

-spec send(Message :: library_protocol:source_message(), Recipient :: pid()) ->
    ok.

send(Message, Recipient) -> % Very bad piece of code
    ok = lager:info("Sending erlang message to process ~p", [Recipient]),
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
    NewSubs = chat_server_message_handler:handle_message(Message, Subs),
    {ok, NewSubs};

websocket_handle(_Data, Subs) ->
    {ok, Subs}.

-spec websocket_info
    ({send, library_protocol:source_message()}, Subs :: state()) ->
        {reply, {text, jiffy:json_value()}, term()};
    ({gproc_ps_event, Event :: binary(), Message :: library_protocol:source_message()}, Subs :: state()) ->
        {reply, {text, jiffy:json_value()}, term()}.

websocket_info({gproc_ps_event, _, Message}, Subs) ->
    ok = lager:info("Websocket info: ~p", [Message]),
    Json = library_protocol:encode(Message),
    {reply, {text, Json}, Subs};

websocket_info({send, Message}, Subs) ->
    ok = lager:info("Websocket info: ~p", [Message]),
    Json = library_protocol:encode(Message),
    {reply, {text, Json}, Subs}.

-spec terminate(_Reason :: term(), _Req :: map(), Subs :: state()) ->
    ok.

terminate(_Reason, _Req, _Subs) ->
    ok = lager:info("Websocket process ~p is terminated", [self()]).
