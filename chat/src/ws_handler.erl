-module(ws_handler).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type state() :: atom().

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%

-spec init({tcp, http}, term(), list()) ->
    {upgrade, protocol, cowboy_websocket}.

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    lager:notice("Initializing websocket, PID: ~p", [self()]),
    {ok, Req, connected}.

websocket_handle({text, Message}, Req, State) ->
    Test = protocol:json_to_server_message(Message, self()),
    lager:info("Decoded JSON: ~p", [Test]),
    case chat_server:send(Test) of
        {error, no_room} ->
            lager:info("Chat room not found"),
            self() ! {send, {error, user}};
        ok ->
            self() ! {send, {success, user}}
    end,
    {ok, Req, State};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({send, Message}, Req, State) ->
    lager:info("Websocket info: ~p", [Message]),
    Json = protocol:message_to_json(Message), % А не ебанет?
    {reply, {text, Json}, Req, State}.

-spec websocket_terminate(term(), term(), state()) ->
    ok.
websocket_terminate(_Reason, _Req, _State) ->
    lager:info("Websocket process ~p is terminated", [self()]),
    ok.
