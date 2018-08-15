%@todo consider moving username (possibly switching to global un) and other data here

-module(chatserv_socket).

%% API
-type socket_state() :: #{
    socket := gen_tcp:socket(),
    joined_rooms := [{non_neg_integer(), pid()}] %pid caching
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
    {ok, #{socket => LSock, joined_rooms => []}, {continue, start_accept}}.

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
handle_info({tcp, ASock, <<Type, RoomId:16/unsigned, Data/binary>>}, State = #{socket := ASock}) ->
    lager:info("Received packet type ~p, directed to: ~p data: ~p", [Type, RoomId, Data]),
    NewState = handle_packet(Type, RoomId, Data, State),
    inet:setopts(ASock, [{active, once}]),
    {noreply, NewState};
handle_info({tcp_closed, ASock}, State = #{socket := ASock}) ->
    lager:info("Tcp connection closed"),
    {stop, shutdown, State}.

%%
%% Internal functions
%%

%%
%@todo possible refactoring

%@todo change format to include room name and use term to bin
-spec handle_packet(non_neg_integer(), non_neg_integer(), binary(), socket_state()) ->
    socket_state().

%system message (only room list for now)
handle_packet(1, 0, _, State = #{socket := Socket}) ->
    RoomList = gen_server:call(room_manager, get_rooms_list),
    ResponseMsg = room_list_to_bin(RoomList),
    gen_tcp:send(Socket, <<0, 1, ResponseMsg/binary>>),
    State;
%join room
handle_packet(2, RoomId, _, State = #{socket := Socket, joined_rooms := Rooms}) ->
    RoomList = gen_server:call(room_manager, get_rooms_list),
    {RoomName, Pid} = lists:keyfind(RoomId, 1, RoomList),
    gen_server:cast(Pid, {join_room, self()}),
    gen_tcp:send(Socket, <<0, 2, RoomId:16/unsigned>>),
    State#{joined_rooms := [{RoomName, Pid} | Rooms]};
%set name
handle_packet(3, RoomId, <<Name/binary>>, State = #{socket := Socket, joined_rooms := Rooms}) ->
    gen_server:cast(room_pid_by_id(RoomId, Rooms), {set_name, self(), binary_to_list(Name)}),
    gen_tcp:send(Socket, <<0, 3, RoomId:16/unsigned, Name/binary>>),
    State;
%send message
handle_packet(4, RoomId, <<Message/binary>>, State = #{socket := Socket, joined_rooms := Rooms}) ->
    gen_server:cast(room_pid_by_id(RoomId, Rooms), {send_message, self(), binary_to_list(Message)}),
    gen_tcp:send(Socket, <<0, 4, RoomId:16/unsigned, Message/binary>>),
    State.


%room_list_to_bin
-spec room_list_to_bin([{non_neg_integer(), pid()}]) ->
    binary().
room_list_to_bin(RoomList) ->
    room_list_to_bin(RoomList, []).

room_list_to_bin([], Stack) ->
    list_to_binary(lists:reverse(Stack));
room_list_to_bin([{Id, _}|T], Stack) ->
    room_list_to_bin(T, [Id | Stack]).

%room_pid_by_id
-spec room_pid_by_id(non_neg_integer(), [{non_neg_integer(), pid()}]) ->
    pid().
room_pid_by_id(RoomId, Rooms) ->
    {_, Pid} = lists:keyfind(RoomId, 1, Rooms),
    Pid.