-module(ws_handler).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([send/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec send(Message :: protocol:source_message(), Recipient :: pid()) ->
    ok.

send(Message, Recipient) ->
    ok = lager:info("Sending erlang message to process ~p", [Recipient]),
    Recipient ! {send, Message},
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(Req :: cowboy_req:req(), Opts :: cowboy_websocket:opts()) ->
    {cowboy_websocket, cowboy_req:req(), cowboy_websocket:opts()}.

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

-spec websocket_init(term()) ->
    {ok, connected}.

websocket_init(_) ->
    ok = lager:notice("Initializing websocket, PID: ~p", [self()]),
    PID = spawn_link(client_code, init, [self()]),
    {ok, PID}.

-spec websocket_handle({text, Json :: jiffy:json_value()}, PID :: term()) ->
    {ok, term()}.

websocket_handle({text, Json}, PID) ->
    Message = protocol:decode(Json),
    %chat_room:send(Message, self()),
    PID ! {websocket, Message},
    {ok, PID};

websocket_handle(_Data, PID) ->
    {ok, PID}.

-spec websocket_info({send, protocol:source_message()}, PID :: term()) ->
    {reply, {text, jiffy:json_value()}, term()}.

websocket_info({send, Message}, PID) ->
    ok = lager:info("Websocket info: ~p", [Message]),
    Json = protocol:encode(Message),
    {reply, {text, Json}, PID}.

-spec terminate(_Reason :: term(), _Req :: map(), PID :: term()) ->
    ok.

terminate(_Reason, _Req, PID) ->
    PID ! stop,
    ok = lager:info("Websocket process ~p is terminated", [self()]).
