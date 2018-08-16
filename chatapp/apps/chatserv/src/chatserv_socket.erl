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

-spec handle_continue(start_accept, socket_state()) ->
    {noreply, socket_state()}.

handle_continue(start_accept, State = #{socket := LSock}) ->
    case gen_tcp:accept(LSock) of
        {ok, ASock} ->
            lager:info("New client connected ~p", [ASock]),
            gen_server:cast(socket_manager, {client_connected, self()}),

            {noreply, State#{socket := ASock}};
        _ ->
            {stop, shutdown, State}
    end.

-spec handle_cast({tcp_send, chatlib_sock:packet_type()}, socket_state()) ->
    {noreply, socket_state()}.
handle_cast({tcp_send, Message}, State = #{socket := ASock}) ->
    Data = chatlib_sock:encode(Message),
    lager:info("Sending packet ~p -> ~p", [self(), Data]),

    gen_tcp:send(ASock, Data),
    {noreply, State}.

-spec handle_info(
    {tcp, gen_tcp:socket(), any()} | {tcp_closed, gen_tcp:socket()},
    socket_state()) ->
    {noreply, socket_state()}.
handle_info({tcp, ASock, Data}, State = #{socket := ASock}) ->
    Message = chatlib_sock:decode(Data),
    NewState = handle_packet(Message, State),

    lager:info("Received packet ~p", [Message]),
    inet:setopts(ASock, [{active, once}]),
    {noreply, NewState};
handle_info({tcp_closed, ASock}, State = #{socket := ASock, joined_rooms := Rooms}) ->
    lists:foreach(
        fun({_, Pid}) ->
            gen_server:cast(Pid, {leave_room, self()})
        end,
        Rooms
    ),
    lager:info("Tcp connection with ~p closed", [self()]),
    {stop, shutdown, State}.

-spec handle_call(any(), any(), socket_state()) ->
    {noreply, socket_state()}.
handle_call(_, _, State) ->
    {noreply, State}.

%%
%% Internal functions
%%

%@todo change format to include room name and use term to bin
-spec handle_packet(chatlib_sock:packet_types(), socket_state()) ->
    socket_state().

handle_packet(get_rooms_list, State = #{socket := Socket}) ->
    RoomList = gen_server:call(room_manager, get_rooms_list),

    Response = chatlib_sock:encode({server_response, {0, prep_room_list(RoomList)}}),
    gen_tcp:send(Socket, Response),

    State;

handle_packet({join_room, NewRoomId}, State = #{socket := Socket, joined_rooms := Rooms}) ->
    RoomList = gen_server:call(room_manager, get_rooms_list),
    {RoomId, Pid} = lists:keyfind(NewRoomId, 1, RoomList),
    gen_server:cast(Pid, {join_room, self()}),

    Response = chatlib_sock:encode({server_response, {0, RoomId}}),
    gen_tcp:send(Socket, Response),

    State#{joined_rooms := [{RoomId, Pid} | Rooms]};

handle_packet({set_name, RoomId, Name}, State = #{socket := Socket, joined_rooms := Rooms}) ->
    gen_server:cast(room_pid_by_id(RoomId, Rooms), {set_name, self(), Name}),

    Response = chatlib_sock:encode({server_response, {0, RoomId, Name}}),
    gen_tcp:send(Socket, Response),

    State;

handle_packet({send_message, RoomId, Message}, State = #{socket := Socket, joined_rooms := Rooms}) ->
    gen_server:cast(room_pid_by_id(RoomId, Rooms), {send_message, self(), Message}),

    Response = chatlib_sock:encode({server_response, {0, RoomId, Message}}),
    gen_tcp:send(Socket, Response),

    State.

%room_list_to_bin
-spec prep_room_list([{non_neg_integer(), pid()}]) ->
    [non_neg_integer()].
prep_room_list(RoomList) ->
    prep_room_list(RoomList, []).

prep_room_list([], Stack) ->
    lists:reverse(Stack);
prep_room_list([{Id, _} | T], Stack) ->
    prep_room_list(T, [Id | Stack]).

%room_pid_by_id
-spec room_pid_by_id(non_neg_integer(), [{non_neg_integer(), pid()}]) ->
    pid().
room_pid_by_id(RoomId, Rooms) ->
    {_, Pid} = lists:keyfind(RoomId, 1, Rooms),
    Pid.