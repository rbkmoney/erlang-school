-module(chat_server_app).

-behaviour(application).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/2]).
-export([stop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start(Type :: term(), Args :: term()) ->
    {ok , pid()}.

start(_Type, _Args) ->
    {ok, Host} = application:get_env(chat_server, host),
    {ok, Port} = application:get_env(chat_server, port),
    ok = lager:debug("Application will run on ~p:~p", [Host, Port]),
    {ok, _} = chat_server_sup:start_link(Host, Port).

-spec stop(_State :: term()) ->
    ok.

stop(_State) ->
    ok.
