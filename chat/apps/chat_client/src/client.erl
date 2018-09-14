-module(client).

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

-type status() :: connected | not_connected | registered.
-type message_list() :: [protocol:source_message()].
-type state() :: state(status()).
-type state(Status) :: #{
    pid => pid(),
    status => Status,
    username => binary(),
    messageList => message_list()
}.
-type connected_state() :: state(connected).
-type registered_state() :: state(registered).
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

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec add_message(Json :: jiffy:json_value(), State :: state()) ->
    state().

add_message(Json, State) ->
    Message = protocol:decode(Json),
    ok = lager:info("Adding message ~p to mesageList", [Message]),
    #{messageList := MessageList} = State,
    NewMessageList = [Message | MessageList],
    State#{messageList => NewMessageList}.

-spec get_messages_(State :: state()) ->
    state().

get_messages_(State) ->
    maps:get(messageList, State).

-spec connection_info(State :: state()) ->
    {binary(), pid()}.

connection_info(State) ->
    {username(State), pid(State)}.

-spec update_status(NewStatus :: status(), State :: state()) ->
    state().

update_status(NewStatus, State) ->
    State#{status => NewStatus}.

-spec username(State :: state()) ->
    binary().

username(State) ->
    maps:get(username, State).

-spec set_username_(Username :: binary(), State :: state()) ->
    state().

set_username_(Username, State) ->
    State#{username => Username}.

-spec pid(State :: state()) ->
    pid().

pid(State) ->
    maps:get(pid, State, not_connected).

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
    NewState = update_status(connected, State),
    NewState#{pid => Pid}.

-spec format_message(Json ::  jiffy:json_value()) ->
    ok.

format_message(Json) -> % Optional output
    {Event, Username, Message, _} = protocol:decode(Json),
    case Event of
        send_message ->
            ok = io:fwrite("~p: ~p~n", [binary_to_list(Username), binary_to_list(Message)]);
        success ->
            ok;
        error ->
            ok = io:fwrite("~p ~p~n", [Event, Message]);
        _ ->
            ok = io:fwrite("~p ~p this room~n", [binary_to_list(Username), Event])
    end.

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
        {reply, ok, registered_state()};
    (get_messages, _From :: term(), State :: state()) ->
        {reply, message_list(), state()};
    (stop, _From :: term(), State :: state()) ->
        {reply, ok, state()}.

handle_call({connect, Host, Port}, _From, #{status := not_connected} = State) ->
    NewState = ws_connect(Host, Port, State),
    {reply, ok, NewState};

handle_call({connect, Host, Port}, _From, State) ->
    {reply, already_connected, NewState};

handle_call({set_username, Username}, _From, State) ->
    NewState = set_username_(Username, State),
    {reply, ok, NewState};

handle_call({join, RoomId}, _From, #{status := not_connected} = State) ->
    {reply, denied, State};

handle_call({join, RoomId}, _From, State) ->
    {Username, PID} = connection_info(State),
    ok = lager:info("User with pid ~p wants to join room ~p", [PID, RoomId]),
    Message = protocol:encode({register, Username, <<>>, RoomId}),
    ok = lager:info("Sending message ~p throught websocket", [Message]),
    ok = gun:ws_send(PID, {text, Message}),
    NewState = update_status(registered, State),
    {reply, ok, NewState};

handle_call(stop, _From, State) -> % Stopping connection will cause leaving the server.
    PID = pid(State),
    ok = gun:close(PID),
    NewState = update_status(not_connected, State),
    {reply, ok, NewState};

handle_call(get_messages, _From, State) ->
    ok = lager:info("User ~p asked for received messages", [username(State)]),
    MessageList = get_messages_(State),
    {reply, MessageList, State};

handle_call(_, _, State) ->
    {reply, ok, State}.

-spec handle_cast({send_message, {Message :: binary(),
    RoomId :: atom()}}, State ::registered_state()) ->
        {noreply, registered_state()}.

handle_cast({send_message, {Message, RoomId}}, #{status := registered} = State) ->
        {Username, PID} = connection_info(State),
        Username = username(State),
        Json = protocol:encode({send_message, <<>>, Message, RoomId}),
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
