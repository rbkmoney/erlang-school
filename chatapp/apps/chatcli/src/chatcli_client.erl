-module(chatcli_client).
-behaviour(gen_server).

%% API
-export([start_link/2]).

-export([
    join_room/1,
    set_name/2,
    send_message/2,
    await_response/2,
    has_message/3
]).

%% gen_server

-type addr() :: { inet:hostname(), inet:port_number() }.
-type state() :: #{
    conn_addr := addr(),
    connpid := pid() | undefined,
    conn_state := connecting | upgrading | ready | waiting,
    last_response := chatlib_proto:response_code(),
    message_history := #{ chatlib_proto:room_id_direct() => chatlib_proto:message_list() }
}.

-export([
    init/1,
    handle_continue/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(SERVER, ?MODULE).

%%%
%%% API
%%%
start_link(Ip, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Ip, Port}, []).

-spec join_room(chatlib_proto:room_id_direct()) ->
    ok.
join_room(RoomId) ->
    gen_server:call(?SERVER, {join_room, RoomId}).

-spec set_name(chatlib_proto:room_id_direct(), chatlib_proto:member_name()) ->
    ok.
set_name(RoomId, Name) ->
    gen_server:call(?SERVER, {set_name, RoomId, Name}).

-spec send_message(chatlib_proto:room_id_direct(), chatlib_proto:message_text()) ->
    ok.
send_message(RoomId, Message) ->
    gen_server:call(?SERVER, {send_message, RoomId, Message}).

-spec await_response(timeout(), pos_integer()) ->
    chatlib_proto:response_code() | timeout.
await_response(_, 0) ->
    timeout;
await_response(WaitMs, Retries) ->
    case gen_server:call(?SERVER, get_response) of
        waiting -> timer:sleep(WaitMs), await_response(WaitMs, Retries-1);
        {ready, Response} -> Response
    end.

-spec has_message(chatlib_proto:room_id_direct(), chatlib_proto:room_name(), chatlib_proto:message_text()) ->
    boolean().
has_message(RoomId, Name, Message) ->
    gen_server:call(?SERVER, {has_message, RoomId, Name, Message}).

%%%
%%% gen_server
%%%
-spec init(addr()) ->
    {ok, state(), {continue, connect}}.
init(Addr) ->
    {ok, #{
        conn_addr => Addr,
        connpid => undefined,
        conn_state => connecting,
        last_response => ok,
        message_history => #{}
    }, {
        continue, connect
    }}.

-spec handle_continue(connect, state()) ->
    {noreply, state()}.
handle_continue(connect, State = #{conn_addr := {Ip, Port}, conn_state := connecting}) ->
    {ok, ConnPid} = gun:open(Ip, Port),

    {noreply, State#{connpid := ConnPid}}.

-spec handle_call(any(), any(), state()) ->
    {reply, ok, state()}.
handle_call(Msg = {join_room, _}, _, State) ->
    encode_and_send(Msg, State);

handle_call(Msg = {set_name, _, _}, _, State) ->
    encode_and_send(Msg, State);

handle_call(Msg = {send_message, _, _}, _, State) ->
    encode_and_send(Msg, State);

handle_call(get_response, _, State = #{conn_state := waiting}) ->
    {reply, waiting, State};

handle_call(get_response, _, State = #{conn_state := ready, last_response := Response}) ->
    {reply, {ready, Response}, State};

handle_call({has_message, RoomId, Name, Message}, _, State = #{message_history := MsgHis}) ->
    RoomMsgs = maps:get(RoomId, MsgHis, []),

    Result = lists:foldl(
        fun({_, MName, MMessage}, _Res) ->
            (Name == MName) and (MMessage == Message)
        end,
        false, RoomMsgs
    ),

    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(any(), state()) ->
    {noreply, state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(
        {gun_upgrade, pid(), _, _, _} |
        {gun_response, pid(), _, _, _, _} |
        {gun_up, pid(), _} |
        {gun_error, pid(), _, _} |
        {gun_ws, pid(), _, {text, binary()}},
        state()
    ) ->
    {noreply, state()}.
handle_info({gun_ws, ConnPid, _, {text, Data}}, State = #{connpid := ConnPid}) ->
    Message = chatlib_proto:decode(Data),
    NewState = handle_ws(Message, State),

    {noreply, NewState};

handle_info({gun_upgrade, ConnPid, _, [<<"websocket">>], _}, State = #{connpid := ConnPid, conn_state := upgrading}) ->
    io:format("Updgraded!"),

    {noreply, State#{conn_state => ready}};

handle_info({gun_response, ConnPid, _, _, _, _}, State = #{connpid := ConnPid}) ->
    io:format("Error!"),
    {noreply, State};

handle_info({gun_up, ConnPid, _}, State = #{connpid := ConnPid, conn_state := connecting}) ->
    io:format("Connected!"),

    _ = gun:ws_upgrade(ConnPid, "/ws"),
    {noreply, State#{conn_state => upgrading}};

handle_info({gun_error, ConnPid, _, _}, State = #{connpid := ConnPid}) ->
    io:format("Error!"),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%
%% Internal
%%

-spec handle_ws(chatlib_proto:packet(), Old :: state()) ->
    New :: state().
handle_ws({receive_messages, RoomId, MessageList}, State) ->
    add_messages_to_history(RoomId, MessageList, State);

handle_ws({server_response, _, ResponseCode}, State = #{conn_state := waiting}) ->
    State #{conn_state => ready, last_response => ResponseCode }.

-spec add_messages_to_history(chatlib_proto:room_id_direct(), chatlib_proto:message_list(), state()) ->
    state().
add_messages_to_history(RoomId, NewMessages, State = #{message_history := History}) ->
    OldMsgs = maps:get(RoomId, History, []),
    NewMsgs = NewMessages ++ OldMsgs,
    NewHistory = maps:put(RoomId, NewMsgs, History),

    State #{message_history := NewHistory}.

-spec encode_and_send(any(), state()) ->
    {reply, ok, state()}.
encode_and_send(Msg, State = #{connpid := ConnPid}) ->
    RequestData = chatlib_proto:encode(Msg),

    {reply, ws_send(ConnPid, RequestData), State#{conn_state := waiting}}.

-spec ws_send(pid(), binary()) ->
    ok.
ws_send(Pid, Data) ->
    gun:ws_send(Pid, {text, Data}).