-module(chat_client_client).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(gen_server).

-export([init/1]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([handle_call/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([send/3]).
-export([join/2]).
-export([leave/1]).
-export([connect/3]).
-export([start_link/1]).
-export([get_messages/1]).
-export([set_username/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type status() :: connected | not_connected | joined.
-type message_list() :: [library_protocol:source_message()].
-type state() :: state(status()).
-type state(Status) :: #{
    pid => pid(),
    status => Status,
    username => binary(),
    messageList => message_list()
}.
-type connected_state() :: state(connected).
-type joined_state() :: state(joined).
-type not_connected_state() :: state(not_connected).
-type host() :: string().
-type connection_port() :: non_neg_integer().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link(Id :: binary()) ->
    {ok, pid()}.

start_link(Id) ->
    {ok, _} = gen_server:start_link({global, Id}, ?MODULE, <<"Incognito">>, []).

-spec connect(Host :: host(), Port :: connection_port(), Id :: binary()) ->
    ok.

connect(Host, Port, Id) ->
    ok = gen_server:call({global, Id}, {connect, Host, Port}).

-spec send(Id :: binary(), Message :: binary(), RoomId :: binary()) ->
    ok.

send(Id, Message, RoomId) ->
    ok = gen_server:cast({global, Id}, {send_message, {Message, RoomId}}).

-spec set_username(Id :: binary(), Username :: binary()) ->
    ok.

set_username(Id, Username) ->
    ok = gen_server:call({global, Id}, {set_username, Username}).

-spec join(Id :: binary(), RoomId :: binary()) ->
    ok.

join(Id, RoomId) ->
    ok = gen_server:call({global, Id}, {join, RoomId}).

leave(Id) -> % Might be a really bad idea, to stop like this
    ok = gen_server:call({global, Id}, stop).

-spec get_messages(Id :: binary()) ->
    message_list().

get_messages(Id) ->
    gen_server:call({global, Id}, get_messages).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(Username :: string()) ->
    {ok, state()}.

init(Username) ->
    {ok, #{status => not_connected, username => Username, messageList => []}}.

-spec handle_call
    ({connect, Host :: host(), Port :: connection_port()},
     _From :: term(), State:: not_connected_state()) ->
        {reply, ok, connected_state()};
    ({set_username, Username :: binary()}, _From :: term(), State :: state()) ->
        {reply, ok, state()};
    ({join, RoomId :: atom()}, _From :: term(), State :: connected_state()) ->
        {reply, ok, joined_state()};
    (get_messages, _From :: term(), State :: state()) ->
        {reply, message_list(), state()};
    (stop, _From :: term(), State :: state()) ->
        {reply, ok, state()}.

handle_call({connect, Host, Port}, _From, #{status := not_connected} = State) ->
    NewState = ws_connect(Host, Port, State),
    {reply, ok, NewState};

handle_call({connect, _Host, _Port}, _From, State) ->
    {reply, already_connected, State};

handle_call({set_username, Username}, _From, State) ->
    {reply, ok, State#{username => Username}};

handle_call({join, _RoomId}, _From, #{status := not_connected} = State) ->
    {reply, denied, State};

handle_call({join, RoomId}, _From, #{username := Username, pid := PID} = State) ->
    ok = lager:info("User with pid ~p wants to join room ~p", [PID, RoomId]),
    Message = library_protocol:encode({join, Username, <<>>, RoomId}),
    ok = lager:info("Sending message ~p throught websocket", [Message]),
    ok = gun:ws_send(PID, {text, Message}),
    {reply, ok, State#{status => joined}};

handle_call(stop, _From, #{pid := PID} = State) -> % Closing connection will cause leaving the server.
    ok = gun:close(PID),
    {reply, ok, State#{status => not_connected}};

handle_call(get_messages, _From, #{username := Username} = State) ->
    ok = lager:info("User ~p asked for received messages", [Username]),
    MessageList = get_messages_(State),
    {reply, MessageList, State};

handle_call(_, _, State) ->
    {reply, ok, State}.

-spec handle_cast({send_message, {Message :: binary(),
    RoomId :: atom()}}, State ::joined_state()) ->
        {noreply, joined_state()}.

handle_cast({send_message, {Message, RoomId}}, #{status := joined, pid := PID} = State) ->
        Json = library_protocol:encode({send_message, <<>>, Message, RoomId}),
        ok = lager:info("Sending message ~p throught websocket", [Json]),
        ok = gun:ws_send(PID, {text, Json}),
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.

-spec handle_info({gun_ws, _, _, {text, Message :: binary()}}, state()) ->
    {noreply, state()}.

handle_info({gun_ws, _, _, {text, Message}}, State) ->
    ok = lager:info("Caught a message: ~p", [Message]),
    NewState = add_message(Message, State),
    format_message(Message),
    {noreply, NewState};

handle_info(_, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec add_message(Json :: jiffy:json_value(), State :: state()) ->
    state().

add_message(Json, State) ->
    Message = library_protocol:decode(Json),
    ok = lager:info("Adding message ~p to mesageList", [Message]),
    #{messageList := MessageList} = State,
    NewMessageList = [Message | MessageList],
    State#{messageList => NewMessageList}.

-spec get_messages_(State :: state()) ->
    state().

get_messages_(State) ->
    maps:get(messageList, State).

-spec ws_connect(Host :: host(), Port :: connection_port(), State :: state()) ->
    connected_state().

ws_connect(Host, Port, State) ->
    {ok, Pid} = gun:open(Host, Port),
    {ok, _} = gun:await_up(Pid),
    ok = lager:info("Connection to ~p:~p established, perfoming upgrade", [Host, Port]),
    _ = gun:ws_upgrade(Pid, "/websocket"),
    receive
        {gun_upgrade, Pid, _, [<<"websocket">>], _} ->
            ok = lager:info("Success");
        {gun_response, Pid, _, _, Status, Headers} ->
            exit({ws_upgrade_failed, Status, Headers});
        {gun_error, Pid, _, Reason} ->
            exit({ws_upgrade_failed, Reason})
    after 1000 ->
        exit(timeout)
    end,
    State#{pid => Pid, status => connected}.

-spec format_message(Json ::  jiffy:json_value()) ->
    ok.

format_message(Json) -> % Optional output
    {Event, Username, Message, _} = library_protocol:decode(Json),
    case Event of
        send_message ->
            ok = io:fwrite("~p: ~p~n", [binary_to_list(Username), binary_to_list(Message)]);
        error ->
            ok = io:fwrite("~p ~p~n", [Event, Message]);
        _ ->
            ok = io:fwrite("~p ~p this room~n", [binary_to_list(Username), Event])
    end.
