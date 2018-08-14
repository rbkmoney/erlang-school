-module(room2).

-behaviour(gen_server).

-export([start_link/0,init/1,handle_cast/2,handle_call/3, handle_info/2]).

-export([connect/0,accept/2]).

connect() ->
    gen_server:call(?MODULE,connect).


start_link() ->
    gen_server:start_link({local,room2},?MODULE,undefined,[]).

init(undefined) ->
    lager:notice("Initialized room2"),
    {ok, ListenSocket} = gen_tcp:listen(1234, [binary, {active, once}, {packet, 2}]),
    lager:notice("Listening in room2"),
    {ok,#{messages => [], listen_socket => ListenSocket}
    }.

handle_cast({send,_},State) ->
    {noreply,State}.

handle_call(connect,_,State) ->
    ListenSocket = maps:get(listen_socket,State),
    spawn(?MODULE,accept,[self(),ListenSocket]),
    {reply,ok,State}.

handle_info({tcp, Socket, Msg},State) ->
    %Здесь можно поймать сообщение
    Messages = maps:get(messages,State),
    {Username, Text} = binary_to_term(Msg),
    lager:info("Room2 received a message ~p from ~p",[Text,Username]),
    FullMsg = {Username,erlang:localtime(),Text},
    gen_tcp:send(Socket,term_to_binary(FullMsg)),
    NewState = maps:put(messages,[FullMsg | Messages],State),
    {noreply,NewState}.


accept(Parent,ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    gen_tcp:controlling_process(Socket,Parent).
