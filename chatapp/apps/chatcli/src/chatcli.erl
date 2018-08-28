-module(chatcli).
-behaviour(application).

-export([start/2, stop/1]).

%%
%% API
%%

-spec start(any(), any()) ->
    {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    chatcli_client:start_link("localhost", 8888).

-spec stop(any()) ->
    ok.
stop(_State) ->
    ok.
