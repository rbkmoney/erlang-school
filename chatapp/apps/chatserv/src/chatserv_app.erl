-module(chatserv_app).
-behaviour(application).

-export([start/2, stop/1]).

%%
%% API
%%

-spec start(any(), any()) ->
    chatserv_sup:sv_sl_result().
start(_StartType, _StartArgs) ->
    AppConfig = application:get_all_env(chatserv),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/debug", cowboy_static, {priv_file, chatserv, "debug/index.html"}},
            {"/debug/[...]", cowboy_static, {priv_dir, chatserv, "debug"}},

            {"/ws", chatserv_wshandler, []}
        ]}
    ]),

    ListenIp = proplists:get_value(listen_ip, AppConfig, {0, 0, 0, 0}),
    ListenPort = proplists:get_value(listen_port, AppConfig, 8888),

    {ok, _} = cowboy:start_clear(my_http_listener, [
        {ip, ListenIp},
        {port, ListenPort}
    ],
        #{env => #{dispatch => Dispatch}}
    ),

    chatserv_sup:start_link().

-spec stop(any()) ->
    ok.
stop(_State) ->
    ok.
