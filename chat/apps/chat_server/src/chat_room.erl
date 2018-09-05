-module(chat_room).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(gen_server).

-export([init/1]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([handle_info/2]).
-export([terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/1]).
-export([send/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPE EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export_type([username/0]).
-export_type([message/0]).
-export_type([broadcast_message/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type state() :: map().
-type username() :: message().
-type message() :: binary() | string().
-type client_message() :: {atom(), message(), username(), atom()}.
-type websocket_down_message() :: {'DOWN', reference(), process, pid(), term()}.
-type broadcast_message() :: binary().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link(Id :: atom()) ->
    {ok, pid()}.

start_link(Id) ->
    gen_server:start_link({global, Id}, ?MODULE, Id, []).

-spec send(Json :: protocol:json(), Source :: pid()) ->
    no_return().

send(Json, Source) ->
    ClientMessage = protocol:decode(Json),
    RoomId = protocol:room(ClientMessage),
    case room_manager:get_room(RoomId) of
        not_found ->
            Reply = protocol:encode(error, <<"">>, <<"NO ROOM">>, no_room),
            inform(Reply, Source);
        _ ->
            gen_server:cast({global, RoomId}, {client_message, ClientMessage, Source})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec broadcast(Message :: broadcast_message(), State :: map()) ->
    ok.

broadcast(Message, State) ->
    ok = lager:info("Sending message to all users"),
    Connections = maps:get(connections, State),
    RecipientList = maps:keys(Connections),
    [inform(Message, Recipient) || Recipient <- RecipientList],
    ok.

-spec inform(broadcast_message(), pid()) ->
    ok.

inform(Message, Recipient) ->
    ok = lager:info("Sending erlang message to process ~p", [Recipient]),
    Recipient ! {send, Message},
    ok.

-spec this_room(State :: state()) ->
    atom().

this_room(State) ->
    maps:get(room, State).

-spec add_user(PID :: pid(), Username :: username(), State :: state()) ->
    state().

add_user(PID, Username, State) ->
    Connections = maps:get(connections, State),
    NewConnections = maps:put(PID, Username, Connections),
    maps:put(connections, NewConnections, State).

-spec remove_user(PID :: pid(), State :: state()) ->
    state().

remove_user(PID, State) ->
    Connections = maps:get(connections, State),
    NewConnections = maps:remove(PID, Connections),
    maps:put(connections, NewConnections, State).

-spec register_user(Username :: username(), PID :: pid(), State :: state()) ->
    state().

register_user(Username, PID, State) ->
    ok = lager:info("Registration of new user ~p", [Username]),
    NewState = add_user(PID, Username, State),
    erlang:monitor(process, PID),
    Reply = protocol:encode(joined, Username, <<"">>, this_room(State)),
    broadcast(Reply, NewState),
    NewState.

-spec get_user(pid(), state()) ->
    username().
 get_user(PID, State) ->
     Connections = maps:get(connections, State),
     maps:get(PID, Connections, "Incognito").

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(atom()) ->
    {ok, state()}.

init(Id) ->
    process_flag(trap_exit, true),
    ok = lager:notice("Initialized chat room"),
    room_manager:register_room(Id, self()),
    {ok, #{room => Id, connections => #{}}}.

-spec handle_cast
    ({client_message, client_message(), pid()}, State :: state()) ->
        {noreply, state()}.

handle_cast({client_message, {send_message, _Username, Message, RoomId}, Source}, State) ->
    Username = get_user(Source, State),
    ok = lager:info("Chat server got a message ~p from ~p", [Message, Username]),
    Reply = protocol:encode(send_message, Username, Message, RoomId),
    broadcast(Reply, State),
    {noreply, State};

handle_cast({client_message, {register, Username, _Message, _RoomId}, Source}, State) ->
    Success = protocol:encode(success, <<"">>, <<"">>, this_room(State)),
    inform(Success, Source),
    NewState = register_user(Username, Source, State),
    % It actuay works, even if client sends message immediatly after calling for registration, wow!
    {noreply, NewState}.

-spec handle_call(term(), term(), State :: state()) ->
    {reply, ok, state()}.

handle_call(_, _, State) ->
    {reply, ok, State}.

-spec handle_info(websocket_down_message(), state()) ->
    {noreply, state()}.

handle_info({'DOWN', _, process, PID, _}, State) ->
    Username = get_user(PID, State),
    ok = lager:info("User ~p disconnected", [Username]),
    NewState = remove_user(PID, State),
    RoomId = this_room(State),
    Reply = protocol:encode(left, Username, <<"">>, RoomId),
    broadcast(Reply, NewState),
    {noreply, NewState}.

-spec terminate(normal, state()) ->
    ok.

terminate(_, State) ->
    Reply = protocol:encode(error, <<"">>, <<"Room is terminated">>, this_room(State)),
    broadcast(Reply, State),
    ok.
