-module(chat_client_client).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(gen_server).

-export([init       /1]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([handle_call/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([send            /3]).
-export([join            /2]).
-export([leave           /2]).
-export([create          /2]).
-export([delete          /2]).
-export([start_link      /0]).
-export([set_username    /2]).
-export([get_last_message/1]).


-define(DEFAULT_USERNAME, <<"Incognito">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type message_list() :: [library_protocol:source_message()].
-type state() :: #{
    pid := pid(),
    connected := boolean(),
    username := binary(),
    message_list := message_list(),
    host := host(),
    port := connection_port()
}.
-type host() :: string().
-type connection_port() :: non_neg_integer().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() ->
    {ok, pid()}.

start_link() ->
    {ok, _} = gen_server:start_link(?MODULE, undefined, []).

-spec send(PID :: pid(), Message :: binary(), RoomId :: binary()) ->
    ok.

send(PID, Message, RoomId) ->
    ok = gen_server:cast(PID, {send_message, Message, RoomId}).

-spec set_username(PID :: pid(), Username :: binary()) ->
    ok.

set_username(PID, Username) ->
    ok = gen_server:call(PID, {set_username, Username}).

-spec join(PID :: pid(), RoomId :: binary()) ->
    ok.

join(PID, RoomId) ->
    ok = gen_server:call(PID, {join, RoomId}).

-spec leave(PID :: pid(), RoomId :: binary()) ->
    ok.

leave(PID, RoomId) ->
    ok = gen_server:call(PID, {leave, RoomId}).

-spec create(PID :: pid(), RoomId :: binary()) ->
    ok.

create(PID, RoomId) ->
    ok = gen_server:call(PID, {create, RoomId}).

-spec delete(PID :: pid(), RoomId :: binary()) ->
    ok.

delete(PID, RoomId) ->
    ok = gen_server:call(PID, {delete, RoomId}).

-spec get_last_message(PID :: pid()) ->
    library_protocol:source_message().

get_last_message(PID) ->
    ok = lager:debug("User ~p called get_last_message", [PID]),
    gen_server:call(PID, pop_message).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(undefined) ->
    {ok, state()}.

init(undefined) ->
    {ok, #{connected => false, host => "localhost", port => 8080, username => ?DEFAULT_USERNAME, message_list => []}}.

-spec handle_call
    ({library_protocol:event(), RoomId :: binary()}, _From :: term(), State :: state()) ->
        {reply, ok, state()};
    (pop_message, _From :: term(), State :: state()) ->
        {reply, library_protocol:source_message(), state()}.

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

handle_call({set_username, Username}, _From, State) ->
    {reply, ok, State#{username => Username}};

handle_call(pop_message, _From, #{username := Username, message_list := MessageList} = State) ->
    ok = lager:info("User ~p asked for received messages", [Username]),
    {Message, Tail} = pop_message(MessageList),
    {reply, Message, State#{message_list => Tail}};

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

handle_info({gun_ws, _, _, {text, Json}}, State) ->
    Message = library_protocol:decode(Json),
    ok = lager:info("Caught a message: ~p", [Message]),
    NewState = push_message(Message, State),
    {noreply, NewState};

handle_info(_, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_request(Event :: library_protocol:event(), RoomId :: binary(), State :: state()) ->
        state().

handle_request(Event, RoomId, #{username := Username} = State) ->
    NewState = ensure_connected(State),
    ok = lager:info("User ~p wants to ~p room ~p", [Username, Event, RoomId]),
    Message = {Event, Username, <<>>, RoomId},
    #{pid := PID} = NewState,
    ok = send_message(Message, PID),
    NewState.

-spec handle_request(send_message, Text :: binary(), RoomId :: binary(), State :: state()) ->
    state().

handle_request(send_message, Text, RoomId, #{username := Username} = State) ->
    NewState = ensure_connected(State),
    ok = lager:info("User ~p wants to send message to room ~p", [Username, RoomId]),
    Message = {send_message, Username, Text, RoomId},
    #{pid := PID} = NewState,
    ok = send_message(Message, PID),
    NewState.

-spec ensure_connected(State :: state()) ->
    state().

ensure_connected(#{connected := Connected} = State) ->
    case Connected of
        true ->
            State;
        false ->
            ws_connect(State)
    end.

-spec send_message(Message :: library_protocol:source_message(), PID :: pid()) ->
    ok.

send_message(Message, PID) ->
    Json = library_protocol:encode(Message),
    ok = lager:info("Sending message ~p throught websocket", [Json]),
    ok = gun:ws_send(PID, {text, Json}).

-spec push_message(Message :: library_protocol:source_message(), State :: state()) ->
    state().

push_message(Message, #{message_list := MessageList} = State) ->
    ok = lager:info("Adding message ~p to message list", [Message]),
    NewMessageList = [Message | MessageList],
    State#{message_list => NewMessageList}.

-spec pop_message([library_protocol:source_message()]) ->
        {library_protocol:source_message(), list()}.

pop_message([]) ->
    {[], []};

pop_message([Head | Tail]) ->
    {Head, Tail}.

-spec ws_connect(State :: state()) ->
    state().

ws_connect(#{connected := false, host := Host, port := Port} = State) ->
    {ok, Pid} = gun:open(Host, Port),
    {ok, _} = gun:await_up(Pid),
    ok = lager:info("Connection to ~p:~p established, perfoming upgrade", [Host, Port]),
    _ = gun:ws_upgrade(Pid, "/websocket"),
    receive
        {gun_upgrade, Pid, _, [<<"websocket">>], _} ->
            ok = lager:info("Upgrade success");
        {gun_response, Pid, _, _, Status, Headers} ->
            exit({ws_upgrade_failed, Status, Headers});
        {gun_error, Pid, _, Reason} ->
            exit({ws_upgrade_failed, Reason})
    after 1000 ->
        exit(timeout)
    end,
    State#{pid => Pid, connected => true}.
