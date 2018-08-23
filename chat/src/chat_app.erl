%%%-------------------------------------------------------------------
%% @doc chat public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_app).

-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
    {'_', [
          {"/", cowboy_static, {priv_file, chat, "index.html"}},
          {"/websocket", ws_handler, []},
          {"/static/[...]", cowboy_static, {priv_dir, chat, "static"}}
          ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]),
    chat_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
