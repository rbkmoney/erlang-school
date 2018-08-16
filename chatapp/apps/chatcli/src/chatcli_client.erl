-module(chatcli_client).
-behavior(gen_server).

%%API
-type client_state() :: #{
    socket := gen_tcp:socket() | undefined
}.

-export([
    get_room_list/0,
    join_room/1,
    set_name/2,
    send_message/2
]).

%% gen_server
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_continue/2,
    handle_cast/2,
    handle_info/2
]).

%%
%% API
%%

get_room_list() ->
    gen_server:cast(chat_client, get_rooms_list).

join_room(RoomId) ->
    gen_server:cast(chat_client, {join_room, RoomId}).

set_name(RoomId, NewName) ->
    gen_server:cast(chat_client, {set_name, RoomId, NewName}).

send_message(RoomId, NewMessage) ->
    gen_server:cast(chat_client, {send_message, RoomId, NewMessage}).

%%
%% gen_server
%%

-spec start_link() ->
    {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, chat_client}, ?MODULE, [], []).

-spec init([]) -> {ok, client_state(), {continue, connect_to_server}}.
init([]) ->
    {ok, #{
        socket => undefined
    }, {
        continue, connect_to_server
    }}.

-spec handle_continue(connect_to_server, client_state()) ->
    {noreply, client_state()}.

handle_continue(connect_to_server, State) ->
    {ok, ConnIp} = application:get_env(chatserv, listen_ip),
    {ok, ConnPort} = application:get_env(chatserv, listen_port),

    {ok, Sock} = gen_tcp:connect(ConnIp, ConnPort, [binary, {active, once}]),

    {noreply, State#{socket:=Sock}}.

-spec handle_cast(chatlib_sock:packet_types(), client_state()) ->
    {noreply, client_state()}.

handle_cast(Message, State = #{socket := Sock}) ->
    Data = chatlib_sock:encode(Message),
    gen_tcp:send(Sock, Data),
    {noreply, State}.

-spec handle_info({tcp, gen_tcp:socket(), binary()} | {tcp_closed, gen_tcp:socket()}, client_state()) ->
    {noreply, client_state()}.

handle_info({tcp, Sock, Data}, State = #{socket := Sock}) ->
    Message = chatlib_sock:decode(Data),
    handle_packet(Message),

    inet:setopts(Sock, [{active, once}]),
    {noreply, State};

handle_info({tcp_closed, Sock}, State = #{socket := Sock}) ->
    {stop, shutdown, State}.

-spec handle_call(any(), any(), client_state()) ->
    {noreply, client_state()}.
handle_call(_, _, State) ->
    {noreply, State}.

%%
%% Internal
%%

-spec handle_packet(chatlib_sock:packet_types()) ->
    ok.

handle_packet({server_response, MessageTerm}) ->
    io:format("~p~n", [MessageTerm]),
    ok;
handle_packet({receive_messages, RoomId, Messages}) ->
    lists:foreach(
        fun({Time, Username, Message}) ->
            io:format("[Client][Room:~p][~p][~p] ~p~n", [RoomId, Time, Username, Message])
        end,
        Messages
    ).



