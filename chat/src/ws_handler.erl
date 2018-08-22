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
	lager:notice("Initializing websocket"),
	lager:info("Process PID is ~p",[self()]),
	{ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
	lager:notice("Caught message: ~p", [Msg]),
	case string:split(Msg, ",") of
		[<<"reg_user:">>, Username] -> %Довольно неприятный костыль, но я не нашел другого способа
			chat_server:register_connection(Username,self());
		_ ->
			chat_server:send(Msg, self())
	end,
	{ok, Req, State};

websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({send, Msg},Req, State) ->
	{reply, {text, Msg}, Req, State}; %Возможно слежует убрать

websocket_info({send_back, Message}, Req, State) ->
	{reply, {text, <<Message/binary>>}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	lager:info("Websocket process ~p is terminated", [self()]),
	ok.
