-module(chatserv_app).
-behaviour(application).

-export([start/2, stop/1]).

%%
%% API
%%

-spec start(any(), any()) ->
    chatserv_sup:sv_sl_result().
start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/debug", cowboy_static, {priv_file, chatserv, "debug/index.html"}},
            {"/debug/[...]", cowboy_static, {priv_dir, chatserv, "debug"}},

            %@todo an actual good webclient here
            {"/chat", cowboy_static, {priv_file, chatserv, "webclient/index.html"}},
            {"/chat/[...]", cowboy_static, {priv_file, chatserv, "webclient"}},

            {"/ws", chatserv_wshandler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_http(http, 100, [
        {port, 8888}
    ], [
        {env, [{dispatch, Dispatch}]}
    ]),

    chatserv_sup:start_link().

-spec stop(any()) ->
    ok.
stop(_State) ->
    ok.
