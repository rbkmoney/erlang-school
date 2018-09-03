-module(chatcli_client).
-behaviour(gen_server).

%% API
-export([start_link/3]).

-export([
    get_rooms/1,
    join_room/2,
    set_name/3,
    send_message/3
]).

%% gen_server

-type message_callback() :: fun((chatlib_proto:room_id(), chatlib_proto:message_list()) -> any()).

-type addr() :: { inet:hostname(), inet:port_number() }.

-type state() :: #{
    conn_addr := addr(),
    connpid := pid() | undefined,
    conn_state := ready | { waiting, {pid(), any()}},
    message_cb := message_callback()
}.

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(SERVER, ?MODULE).

%%%
%%% API
%%%
-spec start_link(inet:hostname(), inet:port_number(), message_callback()) ->
    {ok, pid()} | {error, _}.
start_link(Host, Port, MessageCB) ->
    gen_server:start_link(?MODULE, {{Host, Port}, MessageCB}, []).

-spec get_rooms(pid()) ->
    ok.
get_rooms(Client) ->
    gen_server:call(Client, get_rooms).

-spec join_room(pid(), chatlib_proto:room_id_direct()) ->
    ok.
join_room(Client, RoomId) ->
    gen_server:call(Client, {join_room, RoomId}).

-spec set_name(pid(), chatlib_proto:room_id_direct(), chatlib_proto:member_name()) ->
    ok.
set_name(Client, RoomId, Name) ->
    gen_server:call(Client, {set_name, RoomId, Name}).

-spec send_message(pid(), chatlib_proto:room_id_direct(), chatlib_proto:message_text()) ->
    ok.
send_message(Client, RoomId, Message) ->
    gen_server:call(Client, {send_message, RoomId, Message}).

%%%
%%% gen_server
%%%
-spec init({addr(), message_callback()}) ->
    {ok, state()}.
init({Addr = {Ip, Port}, MessageCB}) ->
    {ok, ConnPid} = gun:open(Ip, Port),
    {ok, _} = gun:await_up(ConnPid),

    _ = gun:ws_upgrade(ConnPid, "/ws"),

    ok = receive
        {gun_upgrade, ConnPid, _, [<<"websocket">>], _} -> ok
         after 2000 -> error(upgrade_timeout)
    end,

    {ok, #{
        conn_addr => Addr,
        connpid => ConnPid,
        conn_state => ready,
        message_cb => MessageCB
    }}.

-spec handle_call(any(), any(), state()) ->
    {reply, ok, state()} | {noreply, state()}.
handle_call(Msg = get_rooms, From, State = #{connpid := ConnPid, conn_state := ready}) ->
    ok = encode_and_send(Msg, ConnPid),
    {noreply, State #{conn_state := {waiting, From}}};

handle_call(Msg = {join_room, _}, From, State = #{connpid := ConnPid, conn_state := ready}) ->
    ok = encode_and_send(Msg, ConnPid),
    {noreply, State #{conn_state := {waiting, From}}};

handle_call(Msg = {set_name, _, _}, From, State = #{connpid := ConnPid, conn_state := ready}) ->
    ok = encode_and_send(Msg, ConnPid),
    {noreply, State #{conn_state := {waiting, From}}};

handle_call(Msg = {send_message, _, _}, From, State = #{connpid := ConnPid, conn_state := ready}) ->
    ok = encode_and_send(Msg, ConnPid),
    {noreply, State #{conn_state := {waiting, From}}};

handle_call(get_response, _, State = #{conn_state := waiting}) ->
    {reply, waiting, State};

handle_call(get_response, _, State = #{conn_state := ready, last_response := Response}) ->
    {reply, {ready, Response}, State}.

-spec handle_cast(any(), state()) ->
    {noreply, state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info({gun_ws, pid(), _, {text, binary()}}, state()) ->
    {noreply, state()}.
handle_info({gun_ws, ConnPid, _, {text, Data}}, State = #{connpid := ConnPid}) ->
    Message = chatlib_proto:decode(Data),
    NewState = handle_ws(Message, State),

    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

%%
%% Internal
%%

-spec handle_ws(chatlib_proto:packet(), Old :: state()) ->
    New :: state().
handle_ws({message_notification, RoomId, MessageList}, State = #{message_cb := MessageCB}) ->
    ok = lager:info("Recived messages ~p ~p~n", [RoomId, MessageList]),

    _ = MessageCB(RoomId, MessageList),

    State;

handle_ws({rooms_notification, global, RoomsList}, State = #{conn_state := {waiting, From}}) ->
    gen_server:reply(From, RoomsList),

    State #{conn_state => ready};

handle_ws({server_response, _, ResponseCode}, State = #{conn_state := {waiting, From}}) ->
    gen_server:reply(From, ResponseCode),

    State #{conn_state => ready}.

-spec encode_and_send(any(), pid()) ->
    ok.
encode_and_send(Msg, ConnPid) ->
    RequestData = chatlib_proto:encode(Msg),
    ws_send(ConnPid, RequestData).

-spec ws_send(pid(), binary()) ->
    ok.
ws_send(Pid, Data) ->
    gun:ws_send(Pid, {text, Data}).