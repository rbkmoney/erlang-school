-module(client).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(gen_server).

-export([init/1]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([handle_call/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([test/0]).
-export([send/3]).
-export([join/2]).
-export([leave/1]).
-export([connect/3]).
-export([start_link/1]).
-export([get_messages/1]).
-export([set_username/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-type state() :: map().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test() ->
    Id = client1,
    Room = room1,
    connected = connect("localhost", 8080, Id),
    set_username(Id, <<"Igor">>),
    join(Id, Room),
    send(Id, <<"Holla">>, Room),
    timer:sleep(200), % Give some time to handle sending and receiving messages,
    get_messages(Id).

start_link(Id) ->
    gen_server:start_link({global, Id}, ?MODULE, "Incognito", []).

connect(Host, Port, Id) -> %переписать
    gen_server:call({global, Id}, {connect, Host, Port}).

send(Id, Message, RoomId) ->
    gen_server:cast({global, Id}, {send_message, {Message, RoomId}}).

set_username(Id, Username) ->
    gen_server:call({global, Id}, {set_username, Username}).

join(Id, RoomId) ->
    gen_server:call({global, Id}, {join, RoomId}).

leave(Id) ->
    gen_server:call({global, Id}, stop).

get_messages(Id) ->
    gen_server:call({global, Id}, get_messages).

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

add_message(Json, State) ->
    Message = protocol:decode(Json),
    lager:info("Adding message ~p to mesageList", [Message]),
    MessageList = maps:get(messageList, State),
    NewMessageList = [Message | MessageList],
    maps:put(messageList, NewMessageList, State).

get_messages_(State) ->
    maps:get(messageList, State).

connection_info(State) ->
    {username(State), pid(State)}.

update_status(NewStatus, State) ->
    maps:put(status, NewStatus, State).

username(State) ->
    maps:get(username, State).

set_username_(Username, State) ->
    maps:put(username, Username, State).

pid(State) ->
    maps:get(pid, State, not_connected).

ws_connect(Host, Port, State) ->
    {ok, Pid} = gun:open(Host, Port),
    {ok, _} = gun:await_up(Pid),
    lager:info("Connection to ~p:~p established, perfoming upgrade", [Host, Port]),
    gun:ws_upgrade(Pid, "/websocket"),
    receive
        {gun_upgrade, Pid, _, [<<"websocket">>], _} ->
            lager:info("Success");
        {gun_response, Pid, _, _, Status, Headers} ->
            exit({ws_upgrade_failed, Status, Headers});
        {gun_error, Pid, _, Reason} ->
            exit({ws_upgrade_failed, Reason})
    after 1000 ->
        exit(timeout)
    end,
    NewState = update_status(connected, State),
    maps:put(pid, Pid, NewState).

format_message(Json) -> % Optional output
    Decoded = protocol:decode(Json),
    Username = protocol:user(Decoded),
    Event = protocol:event(Decoded),
    case Event of
        send_message ->
            Message = protocol:message(Decoded),
            io:fwrite("~p: ~p~n", [binary_to_list(Username), binary_to_list(Message)]);
        success ->
            ok;
        _ ->
            io:fwrite("~p ~p this room~n", [binary_to_list(Username), Event])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

init(Username) ->
    {ok, #{status => not_connected, username => Username, messageList => []}}.

handle_call({connect, Host, Port}, _From, #{status := not_connected} = State) ->
    NewState = ws_connect(Host, Port, State),
    {reply, connected, NewState};

handle_call({set_username, Username}, _From, State) ->
    NewState = set_username_(Username, State),
    {reply, Username, NewState};

handle_call({join, RoomId}, _From, #{status := connected} = State) ->
    {Username, PID} = connection_info(State),
    Message = protocol:encode(register, Username, <<"">>, RoomId),
    lager:info("Sending message ~p throught websocket", [Message]),
    gun:ws_send(PID, {text, Message}),
    NewState = update_status(registered, State),
    {reply, ok, NewState};

handle_call(stop, _From, State) -> % Stopping connection will cause leaving the server.
    PID = pid(State),
    gun:close(PID),
    NewState = update_status(not_connected, State),
    {ok, disconnected, NewState};

handle_call(get_messages, _From, State) ->
    lager:info("User ~p asked for received messages"),
    MessageList = get_messages_(State),
    {reply, MessageList, State};

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast({send_message, {Message, RoomId}}, #{status := registered} = State) ->
        {Username, PID} = connection_info(State),
        Username = username(State),
        EncodedMessage = protocol:encode(send_message, <<"">>, Message, RoomId),
        lager:info("Sending message ~p throught websocket", [EncodedMessage]),
        gun:ws_send(PID, {text, EncodedMessage}),
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.

handle_info({gun_ws, _, _, {text, Message}}, State) ->
    lager:info("Caught a message: ~p", [Message]),
    NewState = add_message(Message, State),
    format_message(Message),
    {noreply, NewState};

handle_info(_, State) ->
    {noreply, State}.
