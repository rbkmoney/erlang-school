-module(chat_client_client).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(gen_server).

-export([init/1]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([handle_call/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([send/3]).
-export([create/2]).
-export([delete/2]).
-export([join/2]).
-export([leave/2]).
-export([start_link/1]).
-export([get_messages/1]).
-export([set_username/2]).


-define(CALL_EVENT_LIST, [join, create, delete, leave]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type message_list() :: [library_protocol:source_message()].
-type state() :: #{
    pid => pid(),
    connected => boolean(),
    username => binary(),
    message_list => message_list(),
    host => host(),
    port => connection_port()
}.
-type host() :: string().
-type connection_port() :: non_neg_integer().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link(Id :: binary()) ->
    {ok, pid()}.

start_link(Id) ->
    {ok, _} = gen_server:start_link({global, Id}, ?MODULE, <<"Incognito">>, []).

-spec send(Id :: binary(), Message :: binary(), RoomId :: binary()) ->
    ok.

send(Id, Message, RoomId) ->
    ok = gen_server:cast({global, Id}, {send_message, Message, RoomId}).

-spec set_username(Id :: binary(), Username :: binary()) ->
    ok.

set_username(Id, Username) ->
    ok = gen_server:call({global, Id}, {set_username, Username}).

-spec join(Id :: binary(), RoomId :: binary()) ->
    ok.

join(Id, RoomId) ->
    ok = gen_server:call({global, Id}, {join, RoomId}).

leave(Id, RoomId) ->
    ok = gen_server:call({global, Id}, {leave, RoomId}).

create(Id, RoomId) ->
    ok = gen_server:call({global, Id}, {create, RoomId}).

delete(Id, RoomId) ->
    ok = gen_server:call({global, Id}, {delete, RoomId}).

-spec get_messages(Id :: binary()) ->
    message_list().

get_messages(Id) ->
    gen_server:call({global, Id}, get_messages).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(Username :: string()) ->
    {ok, state()}.

init(Username) ->
    {ok, #{connected => false, host => "localhost", port => 8080, username => Username, message_list => []}}.

handle_call({create, RoomId}, _From, State) ->
    NewState = handle_request(create, RoomId, State),
    {reply, ok, NewState};

handle_call({join, RoomId}, _From, State) ->
    NewState = handle_request(join, RoomId, State),
    {reply, ok, NewState};

handle_call({leave, RoomId}, _From, State) ->
    NewState = handle_request(leave, RoomId, State),
    {reply, ok, NewState};

handle_call({delete, RoomId}, _From, State) ->
    NewState = handle_request(delete, RoomId, State),
    {reply, ok, NewState};

handle_call(get_messages, _From, #{username := Username, message_list := MessageList} = State) ->
    % TO DO pop message
    ok = lager:info("User ~p asked for received messages", [Username]),
    {reply, MessageList, State};

handle_call(_, _, State) ->
    {reply, ok, State}.

-spec handle_cast({send_message, Message :: binary(),
    RoomId :: atom()}, State :: state()) ->
        {noreply, state()}.

handle_cast({send_message, Message, RoomId}, State) ->
    NewState = handle_request(send_message, Message, RoomId, State),
    {noreply, NewState};

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

handle_request(Event, RoomId, #{username := Username} = State) ->
    NewState = ensure_connected(State),
    ok = lager:info("User ~p wants to ~p room ~p", [Username, Event, RoomId]),
    Message = {Event, Username, <<>>, RoomId},
    #{pid := PID} = NewState,
    ok = send_message(Message, PID),
    NewState.

handle_request(send_message, Text, RoomId, #{username := Username} = State) ->
    NewState = ensure_connected(State),
    ok = lager:info("User ~p wants to send message to room ~p", [Username, RoomId]),
    Message = {send_message, Username, Text, RoomId},
    #{pid := PID} = NewState,
    ok = send_message(Message, PID),
    NewState.

ensure_connected(#{connected := Connected} = State) ->
    case Connected of
        true ->
            State;
        false ->
            ws_connect(State)
    end.

send_message(Message, PID) ->
    Json = library_protocol:encode(Message),
    ok = lager:info("Sending message ~p throught websocket", [Json]),
    ok = gun:ws_send(PID, {text, Json}).

-spec add_message(Json :: jiffy:json_value(), State :: state()) ->
    state().

add_message(Json, State) ->
    Message = library_protocol:decode(Json),
    ok = lager:info("Adding message ~p to mesageList", [Message]),
    #{message_list := MessageList} = State,
    NewMessageList = [Message | MessageList],
    State#{message_list => NewMessageList}.

-spec ws_connect(State :: state()) ->
    state().

ws_connect(#{connected := false, host := Host, port := Port} = State) ->
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
    State#{pid => Pid, connected => true}.

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
