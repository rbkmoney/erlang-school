%%%-------------------------------------------------------------------
%% @doc coolchat public API
%% @end
%%%-------------------------------------------------------------------

-module(coolchat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

-spec start(any(), any()) ->
    {ok, pid()} | {error, _}.
start(_StartType, _StartArgs) ->
    coolchat_sup:start_link().

%%--------------------------------------------------------------------
-spec stop(any()) ->
    ok.
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
