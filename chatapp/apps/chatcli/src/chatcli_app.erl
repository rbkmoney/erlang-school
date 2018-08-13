-module(chatcli_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%
%% API
%%

-spec start(any(), any()) ->
    {ok, Pid :: pid()} | {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    chatcli_sup:start_link().

-spec stop(any()) ->
    ok.
stop(_State) ->
    ok.
