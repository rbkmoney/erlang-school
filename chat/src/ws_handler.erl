-module(ws_handler).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

%-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-type state() :: term(). % Doesn't matter

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%
%
% -spec init({tcp, http}, term(), list()) ->
%     {upgrade, protocol, cowboy_websocket}.

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_init(_) ->
    lager:notice("Initializing websocket, PID: ~p", [self()]),
    {ok, connected}.

websocket_handle({text, Message}, State) ->
    Test = protocol:json_to_server_message(Message, self()),
    lager:info("Decoded JSON: ~p", [Test]),
    case chat_server:send(Test) of
        {error, no_room} ->
            lager:info("Chat room not found"),
            self() ! {send, {error, <<"NO ROOM">>}};
        ok ->
            self() ! {send, {success, user}}
    end,
    {ok, State};

websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({send, Message}, State) ->
    lager:info("Websocket info: ~p", [Message]),
    Json = protocol:message_to_json(Message),
    {reply, {text, Json}, State}.

% -spec websocket_terminate(term(), term(), state()) ->
%     ok.
terminate(_Reason, _Req, _State) ->
    lager:info("Websocket process ~p is terminated", [self()]),
    ok.
