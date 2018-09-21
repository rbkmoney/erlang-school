-module(chat_server_app).

-behaviour(application).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/2]).
-export([stop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start(Type :: term(), Args :: term()) ->
    {ok , pid()}.

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
    {'_', [
            {"/", cowboy_static, {priv_file, chat_server, "index.html"}},
            {"/websocket", chat_server_ws_handler, []},
            {"/static/[...]", cowboy_static, {priv_dir, chat_server, "static"}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    {ok, _} = chat_server_sup:start_link().

-spec stop(_State :: term()) ->
    ok.

stop(_State) ->
    ok.
