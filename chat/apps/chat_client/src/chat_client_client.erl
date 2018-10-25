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

-define(DEFAULT_USERNAME, <<"Incognito">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type state() :: #{
    pid => pid(),
    username := binary(),
    message_list := message_list(),
    host := host(),
    port := connection_port(),
    pending => pending() % 'From' to send reply to and Message that is expected
}.

-type host() :: string().
-type from() :: {pid(), term()}.
-type connection_port() :: non_neg_integer().
-type message_list() :: [library_protocol:message()].
-type pending() :: {from(), library_protocol:message()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link(Host :: host(), Port :: connection_port()) ->
    {ok, pid()}.

start_link(Host, Port) ->
    {ok, _} = gen_server:start_link(?MODULE, {Host, Port}, []).

-spec send(PID :: pid(), Message :: binary(), RoomId :: library_protocol:room()) ->
    ok.

send(PID, Message, RoomId) ->
    gen_server:call(PID, {send_message, Message, RoomId}).

-spec set_username(PID :: pid(), Username :: binary()) ->
    ok.

set_username(PID, Username) ->
    ok = gen_server:call(PID, {set_username, Username}).

-spec join(PID :: pid(), RoomId :: library_protocol:room()) ->
    ok | {error, not_exists}.

join(PID, RoomId) ->
    gen_server:call(PID, {join, RoomId}).

-spec leave(PID :: pid(), RoomId :: library_protocol:room()) ->
    ok.

leave(PID, RoomId) ->
    gen_server:call(PID, {leave, RoomId}).

-spec create(PID :: pid(), RoomId :: library_protocol:room()) ->
    ok.

create(PID, RoomId) ->
    gen_server:call(PID, {create, RoomId}).

-spec delete(PID :: pid(), RoomId :: library_protocol:room()) ->
    ok.

delete(PID, RoomId) ->
    gen_server:call(PID, {delete, RoomId}).

-spec get_last_message(PID :: pid()) ->
    library_protocol:message().

get_last_message(PID) ->
    ok = lager:debug("Process ~p called get_last_message", [PID]),
    gen_server:call(PID, pop_message).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init({Host :: host(), Port :: connection_port()}) ->
    {ok, state()}.

init({Host, Port}) ->
    State = ws_connect(#{
        host => Host,
        port => Port,
        username => ?DEFAULT_USERNAME,
        message_list => []
    }),
    {ok, State}.

-spec handle_call
    ({library_protocol:event() | set_username, RoomId :: library_protocol:room()}, From :: from(), State :: state()) ->
        {reply, ok, state()};
    (pop_message, From :: from(), State :: state()) ->
        {reply, library_protocol:message() | undefined, state()}.


handle_call({set_username, Username}, _From, State) ->
    {reply, ok, State#{username => Username}};

handle_call({Event, RoomId}, From, #{username := Username} = State) ->
    ok = lager:debug("Client ~p triggered {~p, ~p}", [Username, Event, RoomId]),
    NewState = handle_request(Event, RoomId, From, State),
    {noreply, NewState};

handle_call({send_message, Message, RoomId}, From, State) ->
    NewState = handle_request({message, Message}, RoomId, From, State),
    {noreply, NewState};

handle_call(pop_message, _From, #{username := Username, message_list := MessageList} = State) ->
    ok = lager:debug("Client ~p asked for received messages", [Username]),
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

handle_info({gun_ws, _, _, {text, Json}}, #{pending := {From, ExpectedMessage}, username := Username} = State) ->
    Message = library_protocol:decode(Json),
    ok = lager:info("Client ~p Caught a message: ~p", [Username, Message]),
    ok = lager:info("Message ~p is currently pending", [ExpectedMessage]),
    NewState = case Message of
        ExpectedMessage ->
            NewState0 = push_message(Message, State),
            gen_server:reply(From, ok),
            maps:remove(pending, NewState0);
        Error = {error, _} ->
            gen_server:reply(From, Error),
            maps:remove(pending, State);
        _ ->
            State
    end,
    {noreply, NewState};

handle_info({gun_ws, _, _, {text, Json}}, #{username := Username} = State) ->
    Message = library_protocol:decode(Json),
    ok = lager:info("Client ~p Caught a message: ~p", [Username, Message]),
    NewState = push_message(Message, State),
    {noreply, NewState};

handle_info(_, State) ->
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_request(library_protocol:event(), library_protocol:room(), {pid(), term()}, state()) ->
    state().

handle_request(Event, RoomId, From, #{username := Username} = State) ->
    ok = lager:info("User ~p wants to send message to room ~p", [Username, RoomId]),
    Message = {Event, Username, RoomId},
    ok = lager:debug("Message being sent: ~p", [Message]),
    #{pid := PID} = State,
    ok = send_message(Message, PID),
    State#{pending => {From, Message}}.

-spec send_message(Message :: library_protocol:message(), PID :: pid()) ->
    ok.

send_message(Message, PID) ->
    Json = library_protocol:encode(Message),
    ok = lager:info("Sending message ~p throught websocket", [Json]),
    ok = gun:ws_send(PID, {text, Json}).

-spec push_message(Message :: library_protocol:message(), State :: state()) ->
    state().

push_message(Message, #{message_list := MessageList} = State) ->
    NewMessageList = [Message | MessageList],
    State#{message_list => NewMessageList}.

-spec pop_message([library_protocol:message()]) ->
        {library_protocol:message() | undefined, list()}.

pop_message([]) ->
    {undefined, []};

pop_message([Head | Tail]) ->
    {Head, Tail}.

-spec ws_connect(State :: state()) ->
    state().

ws_connect(#{host := Host, port := Port} = State) ->
    {ok, Pid} = gun:open(Host, Port, #{retry => 0}),
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
    link(Pid),
    State#{pid => Pid}.
