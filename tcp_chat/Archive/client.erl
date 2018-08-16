-module(client).

-export([start/0,start/1,start/3,client/3,send/2,stop/1,test/0]).

-spec test() ->
    no_return(). %Я не понимаю почему тут no_return, но иначе диалайзер кидает ошибку
test() ->
    PID = start(),
    PID2 = start(),
    send(PID, <<"MESSAGE A">>),
    send(PID2, <<"MESSAGE B">>),
    timer:sleep(500), %Нужно, чтобы дать серверу время на обработку ответов
    stop(PID),
    stop(PID2),
    ok.

-spec start() ->
    no_return().
start() ->
    start("Igor","localhost",1234).

-spec start(list()) ->
no_return().
start(Username) ->
    start(Username,"localhost",1234).

-spec start(list(),list(),non_neg_integer()) ->
    pid().
start(Username,Host, Port) ->
    spawn(?MODULE, client, [Username,Host, Port]). % наверное надо будет переделать на спаунлинк с трапэкзитом

client(Username,Host, Port) ->
    room2:connect(),
    lager:info("Client ~p wants to connect to ~p:~p",[self(),Host,Port]),
    {ok, Socket} = gen_tcp:connect(Host,Port,[binary,{active,true},{packet, 2}]),
    loop(Username,Socket).


-spec send(pid(),binary() | list()) ->
    ok.
send(Pid, Msg) when is_binary(Msg) ->
    Pid ! {send, Msg},
    ok;
send(Pid, Msg) when is_list(Msg) ->
    Pid ! {send, list_to_binary(Msg)},
    ok;
send(_,Msg) ->
    throw({invalid_input,Msg}).

-spec stop(pid()) ->
    ok.
stop(Pid) ->
    Pid ! stop,
    ok.
-spec loop(string(),port()) ->
    ok.
loop(Username,Socket) ->
    receive
        {send, Msg} ->
            lager:info("Client ~p sends message ~p",[self(),Msg]),
            gen_tcp:send(Socket,term_to_binary({Username,Msg})),
            loop(Username,Socket);
        {tcp, Socket, Msg} ->
            DecodedMsg = binary_to_term(Msg),
            lager:info("Client ~p received a message ~p",[self(),DecodedMsg]),
            F = fun(Message) ->
                {Username,Time,Text} = Message,
                {{YY,MM,DD}, {H,M,_}} = Time,
                io:format("~p (~p/~p/~p ~p:~p): ~p~n",[Username,DD,MM,YY,H,M,binary_to_list(Text)])
            end,
            F(DecodedMsg),
            loop(Username,Socket);
        stop ->
            lager:info("Client ~p closes connection", [self()])
        after 500 ->
            loop(Username,Socket)
    end.
