-module(chatserv_app).

-behaviour(application).

-export([start/2, stop/1]).

%%
%% API
%%

-spec start(any(), any()) ->
    chatserv_sup:sv_sl_result().
start(_StartType, _StartArgs) ->
    chatserv_sup:start_link().

-spec stop(any()) ->
    ok.
stop(_State) ->
    ok.
