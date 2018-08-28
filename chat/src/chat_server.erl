-module(chat_server).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(gen_server).

-export([init/1]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/1]).
-export([send/1]).
-export([stop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPE EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export_type([username/0]).
-export_type([message/0]).
-export_type([broadcast_message/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type state() :: map().
-type username() :: message().
-type message() :: binary() | string().
-type client_message() :: {atom(), message() | username(), atom(), pid()}.
-type websocket_down_message() :: {'DOWN', reference(), process, pid(), term()}.
-type broadcast_message() :: {atom(), username(), message()} | {atom(), username()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, Id, []).

send(ClientMessage) ->
    RoomId = protocol:get_room_id(ClientMessage),
    Return = case room_manager:get_room(RoomId) of
        not_found ->
            {error, no_room};
        _ ->
            gen_server:cast(RoomId, {client_message, ClientMessage}),
            ok
    end,
    lager:info("Chat_server:send returns ~p", [Return]),
    Return.

-spec stop(atom()) ->
    stopped | {error, no_room}.

stop(RoomId) ->
    gen_server:call(RoomId, stop),
    stopped.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec broadcast(Message :: broadcast_message(), State :: map()) ->
    ok.

broadcast(Message, State) ->
    lager:info("Sending message to all users"),
    RecipientList = maps:keys(State),
    [inform(Message, Recipient) || Recipient <- RecipientList],
    ok.

-spec inform(broadcast_message(), pid()) ->
    ok.

inform(Message, Recipient) ->
    lager:info("Sending erlang message to process ~p", [Recipient]),
    Recipient ! {send, Message},
    ok.

-spec add_user(PID :: pid(), Username :: username(), State :: state()) ->
    state().

add_user(PID, Username, State) ->
    maps:put(PID, Username, State).

-spec remove_user(PID :: pid(), State :: state()) ->
    state().

remove_user(PID, State) ->
    maps:remove(PID, State).

-spec register_user(Username :: username(), PID :: pid(), State :: state()) ->
    state().

register_user(Username, PID, State) ->
    lager:info("Registration of new user ~p", [Username]),
    NewState = add_user(PID, Username, State),
    erlang:monitor(process, PID),
    Reply = protocol:encode(joined, Username),
    broadcast(Reply, NewState),
    NewState.

-spec get_user(pid(), state()) ->
    username().
 get_user(PID, State) ->
     maps:get(PID, State, "Incognito").

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(atom()) ->
    {ok, state()}.

init(Id) ->
    process_flag(trap_exit, true),
    lager:notice("Initialized chat room"),
    room_manager:register_room(Id, self()),
    {ok, #{}}.

-spec handle_cast
    ({client_message, client_message()}, State :: state()) ->
        {noreply, state()}.

handle_cast({client_message, {send_message, Message, _, Source}}, State) ->
    Username = get_user(Source, State),
    lager:info("Chat server got a message ~p from ~p", [Message, Username]),
    Reply = protocol:encode(message, Username, Message),
    broadcast(Reply, State),
    {noreply, State};

handle_cast({client_message, {register, Username, _, Source}}, State) ->
    NewState = register_user(Username, Source, State),
    % It actuay works, even if client sends message immediatly after calling for registration, wow!
    {noreply, NewState}.

% -spec handle_call(term(), term(), state()) ->
%     {reply, ok, state()}.

handle_call(stop, _From, State) ->
    lager:info("Calling to terminate this room"),
    {stop, normal, State};
handle_call(_, _, State) ->
    {reply, ok, State}.

-spec handle_info(websocket_down_message(), state()) ->
    {noreply, state()}.

handle_info({'DOWN', _, process, PID, _}, State) ->
    Username = get_user(PID, State),
    lager:info("User ~p disconnected", [Username]),
    NewState = remove_user(PID, State),
    Reply = protocol:encode(left, Username),
    broadcast(Reply, NewState),
    {noreply, NewState}.

-spec terminate(normal, state()) ->
    ok.

terminate(_, State) ->
    Reply = protocol:encode(error, <<"Room is terminated">>),
    broadcast(Reply, State),
    ok.
