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
-export([start_link      /2]).
-export([set_username    /2]).
-export([get_last_message/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MACROSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% General

-define(DEFAULT_USERNAME, <<"Incognito">>).
-define(DEFAULT_TIMEOUT,             1000).

% Errors

-define(NOT_SUBSCRIBED_REPLY,  {error, <<>>, <<"NOT JOINED TO THE ROOM">>, <<>>}).
-define(ALREADY_EXISTS_REPLY,     {error, <<>>, <<"ROOM ALREADY EXISTS">>, <<>>}).
-define(ALREADY_SUBSCRIBED_REPLY, {error, <<>>, <<"ALREADY IN THE ROOM">>, <<>>}).
-define(NO_ROOM_REPLY,                        {error, <<>>, <<"NO ROOM">>, <<>>}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type message_list() :: [library_protocol:source_message()].
-type state() :: #{
    pid := pid(),
    connected := boolean(),
    username := binary(),
    message_list := message_list(),
    host := host(),
    port := connection_port(),
    pending := pending() % 'From' to send reply to and Message that is expected
}.
-type active_event() :: join | leave | delete | create.
-type from() :: {pid(), term()}.
-type host() :: string().
-type connection_port() :: non_neg_integer().
-type pending() :: {from(), library_protocol:source_message()} | empty.
-type error() :: already_exists | already_joined | no_room | not_joined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link(Host :: host(), Port :: connection_port()) ->
    {ok, pid()}.

start_link(Host, Port) ->
    {ok, _} = gen_server:start_link(?MODULE, {Host, Port}, []).

-spec send(PID :: pid(), Message :: binary(), RoomId :: binary()) ->
    ok.

send(PID, Message, RoomId) ->
    gen_server:call(PID, {send_message, Message, RoomId}).

-spec set_username(PID :: pid(), Username :: binary()) ->
    ok.

set_username(PID, Username) ->
    ok = gen_server:call(PID, {set_username, Username}).

-spec join(PID :: pid(), RoomId :: binary()) ->
    ok.

join(PID, RoomId) ->
    gen_server:call(PID, {join, RoomId}).

-spec leave(PID :: pid(), RoomId :: binary()) ->
    ok.

leave(PID, RoomId) ->
    gen_server:call(PID, {leave, RoomId}).

-spec create(PID :: pid(), RoomId :: binary()) ->
    ok.

create(PID, RoomId) ->
    gen_server:call(PID, {create, RoomId}).

-spec delete(PID :: pid(), RoomId :: binary()) ->
    ok.

delete(PID, RoomId) ->
    gen_server:call(PID, {delete, RoomId}).

-spec get_last_message(PID :: pid()) ->
    library_protocol:source_message().

get_last_message(PID) ->
    ok = lager:debug("Process ~p called get_last_message", [PID]),
    gen_server:call(PID, pop_message).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init({Host :: host(), Port :: connection_port()}) ->
    {ok, state()}.

init({Host, Port}) ->
    {ok,
        #{
            connected => false,
            host => Host,
            port => Port,
            username => ?DEFAULT_USERNAME,
            message_list => [],
            pending => empty
        }
    }.

-spec handle_call
    ({active_event() | set_username, RoomId :: binary()}, _From :: from(), State :: state()) ->
        {reply, ok, state()};
    (pop_message, _From :: from(), State :: state()) ->
        {reply, library_protocol:source_message() | empty, state()}.

handle_call({create, RoomId}, From, State) ->
    NewState = handle_request(create, RoomId, From, State),
    {noreply, NewState, ?DEFAULT_TIMEOUT};

handle_call({join, RoomId}, From, State) ->
    NewState = handle_request(join, RoomId, From, State),
    {noreply, NewState, ?DEFAULT_TIMEOUT};

handle_call({leave, RoomId}, From, State) ->
    NewState = handle_request(leave, RoomId, From, State),
    {noreply, NewState, ?DEFAULT_TIMEOUT};

handle_call({delete, RoomId}, From, State) ->
    NewState = handle_request(delete, RoomId, From, State),
    {noreply, NewState, ?DEFAULT_TIMEOUT};

handle_call({send_message, Message, RoomId}, From, State) ->
    NewState = handle_request(send_message, Message, RoomId, From, State),
    {noreply, NewState, ?DEFAULT_TIMEOUT};

handle_call({set_username, Username}, _From, State) ->
    {reply, ok, State#{username => Username}};

handle_call(pop_message, _From, #{username := Username, message_list := MessageList} = State) ->
    ok = lager:info("User ~p asked for received messages", [Username]),
    {Message, Tail} = pop_message(MessageList),
    {reply, Message, State#{message_list => Tail}};

handle_call(_, _, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), State :: state()) ->
    {noreply, state()}.

handle_cast(_, State) ->
    {noreply, State}.

-spec handle_info({gun_ws, _, _, {text, Message :: jiffy:json_value()}}, state()) ->
    {noreply, state()}.

handle_info({gun_ws, _, _, {text, Json}}, #{pending := Pending} = State) ->
    Message = library_protocol:decode(Json),
    ok = lager:info("Caught a message: ~p", [Message]),
    case Pending of
        {From, ExpectedMessage} ->
            ok = lager:info("Message ~p is currently pending", [ExpectedMessage]),
            case Message of
                ExpectedMessage ->
                    NewState = push_message(Message, State),
                    gen_server:reply(From, ok);
                _ ->
                    NewState = State#{pending => empty},
                    gen_server:reply(From, match_error(Message))
            end;
        empty ->
            NewState = push_message(Message, State)
    end,
    {noreply, NewState};

handle_info(_, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec match_error(Message :: library_protocol:source_message()) ->
    error().

match_error(Message) ->
    case Message of
        ?NO_ROOM_REPLY ->
            no_room;
        ?ALREADY_EXISTS_REPLY ->
            already_exists;
        ?ALREADY_SUBSCRIBED_REPLY ->
            already_joined;
        ?NOT_SUBSCRIBED_REPLY ->
            not_joined
    end.


-spec handle_request
    (Event :: active_event(), RoomId :: binary(), From :: from(), State :: state()) ->
        state().

handle_request(Event, RoomId, From, #{username := Username} = State) ->
    NewState = ensure_connected(State),
    ok = lager:info("User ~p wants to ~p room ~p", [Username, Event, RoomId]),
    Message = {Event, Username, <<>>, RoomId},
    #{pid := PID} = NewState,
    ok = send_message(Message, PID),
    NewState#{pending => {From, Message}}.

-spec handle_request(send_message, Text :: binary(), RoomId :: binary(), From :: from(), State :: state()) ->
    state().

handle_request(send_message, Text, RoomId, From, #{username := Username} = State) ->
    NewState = ensure_connected(State),
    ok = lager:info("User ~p wants to send message to room ~p", [Username, RoomId]),
    Message = {send_message, Username, Text, RoomId},
    #{pid := PID} = NewState,
    ok = send_message(Message, PID),
    NewState#{pending => {From, Message}}.

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
    NewMessageList = [Message | MessageList],
    State#{message_list => NewMessageList}.

-spec pop_message([library_protocol:source_message()]) ->
        {library_protocol:source_message() | empty, list()}.

pop_message([]) ->
    {empty, []};

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
