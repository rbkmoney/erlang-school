-module(client).

-export([start/0,start/2,client/2,send/2,stop/1,test/0]).

test() ->
    PID = start(),
    PID2 = start(),
    send(PID, <<"MESSAGE A">>),
    send(PID2, <<"MESSAGE B">>),
    timer:sleep(500), %Нужно, чтобы дать серверу время на обруботку ответов
    stop(PID),
    stop(PID2).

start() ->
    start("localhost",1234).

start(Host, Port) ->
    spawn(?MODULE, client, [Host, Port]). % наверное надо будет переделать на спаунлинк с трапэкзитом

client(Host, Port) ->
    lager:info("Client ~p wants to connect to ~p:~p",[self(),Host,Port]),
    {ok, Socket} = gen_tcp:connect(Host,Port,[binary,{active,true},{packet, 2}]),
    loop(Socket).

send(Pid, Msg) when is_binary(Msg) ->
    Pid ! {send, Msg},
    ok;
send(Pid, Msg) ->
    Pid ! {send, list_to_binary(Msg)},
    ok.

stop(Pid) ->
    Pid ! stop,
    ok.

loop(Socket) ->
    receive
        {send, Msg} ->
            lager:info("Client ~p sends message ~p",[self(),Msg]),
            gen_tcp:send(Socket,Msg),
            loop(Socket);
        {tcp, Socket, Msg} ->
            lager:info("Client ~p received a message ~p",[self(),Msg]),
            io:format("New message: ~p~n",[binary_to_list(Msg)]),
            loop(Socket);
        stop ->
            lager:info("Client ~p closes connection", [self()])
        after 500 ->
            loop(Socket)
    end.
