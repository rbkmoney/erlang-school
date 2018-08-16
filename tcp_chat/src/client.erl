-module(client).

-behaviour(gen_server).

-export([start_link/0,start_link/1,init/1,handle_cast/2,handle_call/3,handle_info/2,terminate/2]).

-export([start/0,start/1,send/1,test/0,stop/0]).

-type state() :: #{port => non_neg_integer(), user => list(), host => list()}.

-spec test() ->
    ok.
test() ->
    start("Igor"),
    send("Hello, it's me, Mario!!!"),
    send("Hello, it's me, Luigi!!!"),
    timer:sleep(500),
    stop(),
    room:stop(),
    ok.

start() ->
    start_link().
start(Username) when is_list(Username) ->
    start_link(Username);
start(Username) ->
    {throw({invalid_input,Username})}.

send(Msg) when is_binary(Msg) ->
    lager:info("Casting send message request"),
    gen_server:cast(?MODULE,{send,Msg});
send(Msg) when is_list(Msg) ->
    lager:info("Casting send message request"),
    gen_server:cast(?MODULE,{send,list_to_binary(Msg)});
send(Msg) ->
    {throw({invalid_input,Msg})}.

stop() ->
    gen_server:cast(?MODULE,stop).

start_link() ->
    gen_server:start_link({local,client2},?MODULE,undefined,[]).

start_link(Username) ->
    gen_server:start_link({local,client2},?MODULE,Username,[]).

-spec init(any()) ->
    {ok,state()}.
init(Username) when is_list(Username) ->
    Port = 1234,
    {ok,#{port => Port, user => Username, host => "localhost"}};
init(_) ->
    Port = 1234,
    {ok,#{port => Port, user => "Unknown", host => "localhost"}}.

-spec handle_cast({send,<<>>},state()) ->
    {noreply,state()}.
handle_cast({send,Msg},State) ->
    room2:connect(),
    lager:info("Handling send message"),
    Username = maps:get(user,State),
    Port = maps:get(port,State),
    Host = maps:get(host,State),
    {ok, Socket} = gen_tcp:connect(Host,Port,[binary,{active,true},{packet, 2}]),
    lager:notice("Connected to ~p:~p",[Host,Port]),
    gen_tcp:send(Socket,term_to_binary({Username,Msg})),
    lager:info("Message '~p' sent",[Msg]),
    {noreply,State};
handle_cast(stop,State) ->
    {stop, normal,State}.

-spec handle_call(any(),any(),state()) ->
    {reply,ok,state()}.
handle_call(_,_,State) ->
    {reply,ok,State}.

-spec handle_info({tcp,port(),binary()},state()) ->
    {noreply, state()}.
handle_info({tcp, _Socket, Msg},State) ->
    DecodedMsg = binary_to_term(Msg),
    lager:info("Client ~p received a message ~p",[self(),DecodedMsg]),
    F = fun(Message) ->
        {Username,Time,Text} = Message,
        {{YY,MM,DD}, {H,M,_}} = Time,
        io:format("~p (~p/~p/~p ~p:~p): ~p~n",[Username,DD,MM,YY,H,M,binary_to_list(Text)])
    end,
    F(DecodedMsg),
    {noreply,State}.

-spec terminate(normal,state()) ->
    ok.
terminate(normal,_State) ->
    lager:notice("Terminating client session"),
    ok.
