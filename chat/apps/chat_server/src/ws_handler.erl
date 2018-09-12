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

-spec send(Message :: protocol:source_message(), Pid :: pid()) ->
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
    {ok, connected}.

-spec websocket_handle({text, jiffy:json_value()}, State :: term()) ->
    {ok, term()}.

websocket_handle({text, Json}, State) ->
    Message = protocol:decode(Json),
    chat_room:send(Message, self()),
    {ok, State};

websocket_handle(_Data, State) ->
    {ok, State}.

-spec websocket_info({send, protocol:source_message()}, State :: term()) ->
    {reply, {text, jiffy:json_value()}, term()}.

websocket_info({send, Message}, State) ->
    ok = lager:info("Websocket info: ~p", [Message]),
    Json = protocol:encode(Message),
    {reply, {text, Json}, State}.

-spec terminate(_Reason :: term(), _Req :: map(), State :: term()) ->
    ok.

terminate(_Reason, _Req, _State) ->
    ok = lager:info("Websocket process ~p is terminated", [self()]).
