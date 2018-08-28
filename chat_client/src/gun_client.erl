-module(gun_client).

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
-export([set_username/2]).
-export([start_link/1]).
-export([connect/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%-type state() :: map().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test() ->
    Id = client1,
    Room = room1,
    connected = connect("localhost", 8080, Id),
    set_username(Id, <<"Igor">>),
    join(Id, Room).

start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, "Incognito", []).

connect(Host, Port, Username) -> %переписать
    gen_server:call(Username, {connect, Host, Port}).

send(Id, Message, RoomId) ->
    gen_server:cast(Id, {send_message, {Message, RoomId}}).

set_username(Id, Username) ->
    gen_server:call(Id, {set_username, Username}).

join(Id, RoomId) ->
    gen_server:call(Id, {join, RoomId}).

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

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
    {ok, Protocol} = gun:await_up(Pid),
    lager:info("Connection to ~p:~p established, perfoming upgrade", [Host, Port]),
    gun:ws_upgrade(Pid, "/websocket"),
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], Headers} ->
            lager:info("Success");
        {gun_response, ConnPid, _, _, Status, Headers} ->
            exit({ws_upgrade_failed, Status, Headers});
        {gun_error, ConnPid, StreamRef, Reason} ->
            exit({ws_upgrade_failed, Reason})
    after 1000 ->
        exit(timeout)
    end,
    NewState = update_status(connected, State),
    maps:put(pid, Pid, NewState).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

init(Username) ->
    {ok, #{status => not_connected, username => Username}}.

handle_call({connect, Host, Port}, _From, #{status := not_connected} = State) ->
    NewState = ws_connect(Host, Port, State),
    {reply, connected, NewState};

handle_call({set_username, Username}, _From, State) ->
    NewState = set_username_(Username, State),
    {reply, ok, NewState};

handle_call({join, RoomId}, _From, #{status := connected} = State) ->
    PID = pid(State),
    Username = username(State),
    Message = protocol:mesasge_to_client_json(register, Username, RoomId),
    lager:info("Sending message ~p throught websocket", [Message]),
    gun:ws_send(PID, {text, Message}),
    NewState = update_status(registered, State),
    {reply, ok, State};

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast({send_message, {Message, RoomId}}, #{status := registered} = State) ->
    case pid(State) of
        not_connected ->
            {error, not_connected};
        PID ->
            Username = username(State),
            Message = protocol:mesasge_to_client_json(send_message, Message, RoomId),
            lager:info("Sending message ~p throught websocket", [Message]),
            gun:ws_send(PID, {text, Message})
    end,
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.

handle_info({gun_ws, ConnPid, StreamRef, Frame}, State) ->
    lager:info("Caught a message: ~p", [Frame]),
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.
