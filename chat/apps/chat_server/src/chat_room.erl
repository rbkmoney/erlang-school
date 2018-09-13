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
-export([stop/1]).
-export([send/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type state() :: #{room => binary(), connections => #{pid() => binary()}}.
-type source_message() :: protocol:source_message().
-type websocket_down_message() :: {'DOWN', reference(), process, pid(), term()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link({Id :: binary(), Manager :: pid()}) ->
    {ok, pid()}.

start_link({Id, Manager}) ->
    ok = lager:notice("Chat room ~p start_link", [Id]),
    gen_server:start_link(?MODULE, {Id, Manager}, []).

-spec send(SourceMessage :: source_message(), Source :: pid()) ->
    no_return().

-spec stop(PID :: pid()) ->
    shutdown_ok.

stop(PID) when is_pid(PID) ->
    gen_server:call(PID, stop).

send({_, _, _, RoomId} = SourceMessage, Source) ->
    case room_manager:room_exists(RoomId) of
        false ->
            Reply = {error, <<>>, <<"NO ROOM">>, <<>>},
            ws_handler:send(Reply, Source);
        true ->
            PID = room_manager:room_pid(RoomId),
            gen_server:cast(PID, {source_message, SourceMessage, Source})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec broadcast(Message :: source_message(), State :: state()) ->
    ok.

broadcast(Message, State) ->
    ok = lager:info("Sending message to all users"),
    #{connections := Connections} = State,
    RecipientList = maps:keys(Connections),
    [ws_handler:send(Message, Recipient) || Recipient <- RecipientList],
    ok.

-spec this_room(State :: state()) ->
    binary().

this_room(State) ->
    maps:get(room, State).

-spec add_user(PID :: pid(), Username :: binary(), State :: state()) ->
    state().

add_user(PID, Username, State) ->
    #{connections := Connections} = State,
    NewConnections = Connections#{PID => Username},
    State#{connections => NewConnections}.

-spec remove_user(PID :: pid(), State :: state()) ->
    state().

remove_user(PID, State) ->
    #{connections := Connections} = State,
    NewConnections = maps:remove(PID, Connections),
    State#{connections => NewConnections}.

-spec register_user(Username :: binary(), PID :: pid(), State :: state()) ->
    state().

register_user(Username, PID, State) ->
    ok = lager:info("Registration of new user ~p", [Username]),
    NewState = add_user(PID, Username, State),
    erlang:monitor(process, PID),
    Reply = {joined, Username, <<>>, this_room(State)},
    broadcast(Reply, NewState),
    NewState.

-spec get_user(PID :: pid(), State :: state()) ->
    binary().

 get_user(PID, State) ->
    #{connections := Connections} = State,
    maps:get(PID, Connections, <<"Incognito">>). % Need a degenerative case

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init({Id :: binary(), Manager :: pid()}) ->
    {ok, state()}.

init({Id, Manager}) ->
    Manager ! {register, Id, self()},
    process_flag(trap_exit, true),
    ok = lager:notice("Initialized chat room"),
    {ok, #{room => Id, connections => #{}}}.

-spec handle_cast({source_message, source_message(), pid()}, State :: state()) ->
        {noreply, state()}.

handle_cast({source_message, {send_message, _Username, Message, RoomId}, Source}, State) ->
    Username = get_user(Source, State),
    ok = lager:info("Chat server got a message ~p from ~p", [Message, Username]),
    Reply = {send_message, Username, Message, RoomId},
    broadcast(Reply, State),
    {noreply, State};

handle_cast({source_message, {register, Username, _Message, _RoomId}, Source}, State) ->
    Success = {success, <<>>, <<>>, this_room(State)},
    ok = ws_handler:send(Success, Source),
    NewState = register_user(Username, Source, State),
    % It actuay works, even if client sends message immediatly after calling for registration, wow!
    {noreply, NewState}.

-spec handle_call(stop, term(), State :: state()) ->
    {stop, normal, shutdown_ok, state()}.

handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(_, _, State) ->
    {reply, ok, State}.

-spec handle_info(websocket_down_message(), State :: state()) ->
    {noreply, state()}.

handle_info({'DOWN', _, process, PID, _}, State) ->
    Username = get_user(PID, State),
    ok = lager:info("User ~p disconnected", [Username]),
    NewState = remove_user(PID, State),
    RoomId = this_room(State),
    Reply = {left, Username, <<>>, RoomId},
    broadcast(Reply, NewState),
    {noreply, NewState};

handle_info(Msg, State) ->
    ok = lager:notice("Chat room caught message ~p", [Msg]),
    {noreply, State}.

-spec terminate(term(), State :: state()) ->
    ok.

terminate(_, State) ->
    Reply = {error, <<>>, <<"Room is terminated">>, this_room(State)},
    broadcast(Reply, State),
    ok.
