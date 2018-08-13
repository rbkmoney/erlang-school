-module(chatserv_socket).

%% API
-type socket_state() :: #{
    socket := gen_tcp:socket()
}.

%% gen_server
-behavior(gen_server).
-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_continue/2,
    handle_info/2
]).

%%
%% gen_server
%%

-spec start_link(gen_tcp:socket()) ->
    {ok, pid()} | {error, _}.
start_link(LSock) ->
    gen_server:start_link(?MODULE, LSock, []).

-spec init(gen_tcp:socket()) ->
    {ok, socket_state(), {continue, start_accept}}.
init(LSock) ->
    {ok, #{socket => LSock}, {continue, start_accept}}.

-spec handle_call(any(), any(), socket_state()) ->
    {noreply, socket_state()}.
handle_call(_, _ , State) ->
    {noreply, State}.

-spec handle_cast(tuple(), socket_state()) ->
    {noreply, socket_state()}.
handle_cast({server_reply, Data}, State = #{socket := ASock}) ->
    gen_tcp:send(ASock, Data),
    {noreply, State}.

-spec handle_continue(start_accept, socket_state()) ->
    {noreply, socket_state()}.
handle_continue(start_accept, State = #{socket := LSock}) ->
    {ok, ASock} = gen_tcp:accept(LSock),
    lager:info("New client connected ~p", [ASock]),
    gen_server:cast(socket_manager, {client_connected, self()}),
    {noreply, State#{socket := ASock}}.

-spec handle_info(
        {tcp, gen_tcp:socket(), any()} | {tcp_closed, gen_tcp:socket()},
        socket_state()) ->
    {noreply, socket_state()}.
handle_info({tcp, ASock, <<Type, Rest/binary>>}, State = #{socket := ASock}) ->
    lager:info("Recieved packet type ~p, data: ~p", [Type, Rest]),
    handle_packet(Type, Rest, ASock),
    inet:setopts(ASock, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, ASock}, State = #{socket := ASock}) ->
    lager:info("Tcp connection closed"),
    {stop, shutdown, State}.

%%
%% Internal functions
%%q

handle_packet(1, <<0>>, Socket) ->
    gen_tcp:send(Socket, <<0>>);
handle_packet(2, <<RoomId:16/unsigned>>, Socket) ->
    gen_tcp:send(Socket, <<1, RoomId:16/unsigned>>);
handle_packet(3, <<Name/binary>>, Socket) ->
    gen_tcp:send(Socket, <<1, Name/binary>>);
handle_packet(4, <<Message/binary>>, Socket) ->
    gen_tcp:send(Socket, <<1, Message/binary>>).