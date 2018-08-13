-module(room).
-behaviour(gen_server).

-export([room/1, accept/2]).
-export([start_link/0,init/1,handle_cast/2,handle_call/3]).

start_link() ->
    gen_server:start_link({local,room},?MODULE,undefined,[]).


init(undefined) ->
    start(),
    {ok,[]}.

start() ->
    start(1234).

start(Port) ->
    spawn(?MODULE,room,[Port]).

room(Port) ->
    lager:info("Room started on port ~p",[Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, 2}]),
    [spawn(?MODULE,accept,[Id,ListenSocket]) || Id <- lists:seq(1,3)],
    receive
        stop_room ->
            exit(stopped)
    end.

accept(Id,ListenSocket) ->
    lager:info("Socket ~p is waiting",[Id]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    lager:info("Socket ~p is now busy",[Id]),
    loop(Id,Socket,ListenSocket).

loop(Id, Socket, ListenSocket) ->
    case gen_tcp:recv(Socket,0) of
        {ok, Msg} ->
            {Username, Text} = binary_to_term(Msg),
            lager:info("Socket ~p received a message ~p",[Id,{Username,Text}]),
            FullMsg = {Username,erlang:localtime(),Text},
            gen_server:cast(?MODULE, {send,FullMsg}),
            gen_tcp:send(Socket,term_to_binary(FullMsg)),
            loop(Id,Socket,ListenSocket);
        {error, closed} ->
            lager:info("Socket ~p stopped",[Socket]),
            accept(Id,ListenSocket)
    end.

handle_cast({send,Message},State) ->
    {noreply,[Message| State]}.

handle_call(_,_,State) ->
    {reply,ok,State}.
