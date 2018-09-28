-module(chat_server_app).

-behaviour(application).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/2]).
-export([stop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type host() :: string() | '_'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start(Type :: term(), Args :: term()) ->
    {ok , pid()}.

start(_Type, Args) ->
    ok = lager:debug("Args are ~p", [Args]),
    {Host, Port} = setup_conneciton_variables(Args),
    {ok, _} = chat_server_sup:start_link(Host, Port).

-spec stop(_State :: term()) ->
    ok.

-spec setup_conneciton_variables(list()) ->
    {host(),  non_neg_integer()}.

setup_conneciton_variables([#{host := Host, port:= Port}]) ->
    {Host, Port};

setup_conneciton_variables(Args)->
    try maps:from_list(Args) of
        ArgsMap ->
            ok = lager:debug("Succesfully transformed list to map"),
            Host = maps:get(host, ArgsMap, '_'),
            Port = maps:get(port, ArgsMap, 8080),
            {Host, Port}
    catch
        error:Error ->
            ok = lager:debug("Caught an error ~p", [Error]),
            Host = '_',
            Port = 8080,
            {Host, Port}
    end.

stop(_State) ->
    ok.
