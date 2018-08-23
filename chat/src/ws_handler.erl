-module(ws_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	lager:notice("Initializing websocket, PID: ~p", [self()]),
	{ok, Req, registration}.

websocket_handle({text, Username}, Req, registration) ->
	lager:info("Registrating user ~p", [Username]),
	chat_server:register_connection(Username, self()),
	{ok, Req, registrated};

websocket_handle({text, Msg}, Req, State = registrated) ->
	lager:info("Cathed message: ~p", [Msg]),
	chat_server:send(Msg, self()),
	{ok, Req, State};

% {register_user, Username} = proto:decode(Msg)

websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({send, Message}, Req, State) ->
	{reply, {text, Message}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	lager:info("Websocket process ~p is terminated", [self()]),
	ok.
