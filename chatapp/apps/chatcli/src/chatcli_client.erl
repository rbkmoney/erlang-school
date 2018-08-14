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
    send_message/2]).

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
    gen_server:cast(chat_client, get_room_list).

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

-spec handle_call(any(), any(), client_state()) ->
    {noreply, client_state()}.
handle_call(_, _, State) ->
    {noreply, State}.

-spec handle_cast(get_room_list |
                {join_room, non_neg_integer()} |
                {set_name, nonempty_string()} |
                {send_message, nonempty_string()},
        client_state()) ->
    {noreply, client_state()}.
handle_cast(get_room_list, State = #{socket := Sock}) ->
    gen_tcp:send(Sock, <<1, 0, 0>>),
    {noreply, State};
handle_cast({join_room, RoomId}, State = #{socket := Sock}) ->
    gen_tcp:send(Sock, <<2, RoomId:16/unsigned, 0>>),
    {noreply, State};
handle_cast({set_name, RoomId, NewName}, State = #{socket := Sock}) ->
    BinaryName = list_to_binary(NewName),
    gen_tcp:send(Sock, <<3, RoomId:16/unsigned, BinaryName/bytes>>),
    {noreply, State};
handle_cast({send_message, RoomId, NewMessage}, State = #{socket := Sock}) ->
    BinaryMessage = list_to_binary(NewMessage),
    gen_tcp:send(Sock, <<4, RoomId:16/unsigned, BinaryMessage/bytes>>),
    {noreply, State}.

-spec handle_continue(connect_to_server, client_state()) ->
    {noreply, client_state()}.
handle_continue(connect_to_server, State) ->
    {ok, Sock} = gen_tcp:connect("0.0.0.0", 8888, [binary, {active, once}]),
    {noreply, State#{socket:=Sock}}.

-spec handle_info({tcp, gen_tcp:socket(), any()} | {tcp_closed, gen_tcp:socket()}, client_state()) ->
    {noreply, client_state()}.
handle_info({tcp, Sock, Data}, State = #{socket := Sock}) ->
    io:format("~p~n", [Data]),
    inet:setopts(Sock, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, Sock}, State = #{socket := Sock}) ->
    lager:info("Tcp connection closed"),
    {stop, shutdown, State}.

%%
%% Internal
%%

