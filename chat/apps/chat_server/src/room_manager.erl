-module(room_manager).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(gen_server).
-export([init/1]).
-export([handle_cast/2]).
-export([handle_call/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([get_room/1]).
-export([get_rooms/0]).
-export([start_link/0]).
-export([create_room/1]).
-export([delete_room/1]).
-export([register_room/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type state() :: #{binary() => pid()}. % RoomName => it's pid

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() ->
    {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

-spec create_room(Id :: binary()) ->
    ok | already_exists.

create_room(Id) ->
    case get_room(Id) of
        not_found ->
            Reply = room_sup:create_room(Id),
            ok = lager:notice("Create room retured ~p", [Reply]),
            Reply;
        _ ->
            already_exists
    end.

-spec register_room(Id :: binary(), PID :: pid()) ->
    ok.

register_room(Id, PID) ->
    ok = lager:notice("Room ~p wants to sign up as ~p", [PID, Id]),
    gen_server:cast(?MODULE, {create, Id, PID}),
    ok.

-spec get_room(Id :: binary()) ->
    pid() | not_found.

get_room(Id) ->
    gen_server:call(?MODULE, {get_room_pid, Id}).

-spec get_rooms() ->
    [binary()].

get_rooms() ->
    gen_server:call(?MODULE, {get_rooms}).

-spec delete_room(Id :: binary()) ->
    deleted | not_found.

delete_room(Id) ->
    gen_server:call(?MODULE, {delete_room, Id}).

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec room_exists(Id :: binary(), State :: state()) ->
    boolean().

room_exists(Id, State) ->
    case find_room(Id, State) of
        not_found ->
            false;
        _ ->
            true
    end.

-spec find_room(Id :: binary(), State :: state()) ->
    pid() | not_found.

find_room(Id, State) ->
    maps:get(Id, State, not_found).

-spec register_room(Id :: binary(), PID :: pid(), State :: state()) ->
    state().

register_room(Id, PID, State) ->
    maps:put(Id, PID, State).

-spec delete_room(Id :: binary(), State :: state()) ->
    state().

delete_room(Id, State) ->
    maps:remove(Id, State).

-spec room_id_list(State :: state()) ->
    [atom()].

room_id_list(State) ->
    maps:keys(State).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(undefined) ->
    {ok, state()}.

init(undefined) ->
    ok = lager:notice("Initialized room manager"),
    {ok, #{}}.

-spec handle_cast
    ({create, Id :: binary(), PID :: pid()}, State :: state()) ->
        {noreply, state()}.

handle_cast({create, Id, PID}, State) ->
    case room_exists(Id, State) of
        false ->
            NewState = register_room(Id, PID, State),
            ok = lager:info("Room ~p succesfully signed up in manager", [Id]);
        true ->
            ok = lager:info("Room ~p already exists", [Id]),
            NewState = State
    end,
    {noreply, NewState}.

-spec handle_call
    ({get_rooms}, _From :: {pid(), reference()}, State :: state()) -> % Tag is taken from docs
        {reply, [binary()], state()};
    ({get_room_pid, Id :: binary()}, _From :: {pid(), reference()}, State :: state()) ->
        {reply, pid() | not_found, state()};
    ({delete_room, Id :: binary()}, _From :: {pid(), reference()}, State :: state()) ->
        {reply, ok | {error, room_sup:error()} | not_found, state()}.


handle_call({get_rooms}, _From, State) ->
    Rooms = room_id_list(State),
    {reply, Rooms, State};

handle_call({get_room_pid, Id}, _From, State) ->
    ok = lager:info("Searching for room ~p", [Id]),
    RoomPID = find_room(Id, State),
    {reply, RoomPID, State};

handle_call({delete_room, Id}, _From, State) ->
    NewState = case room_exists(Id, State) of
        true ->
            ok = lager:notice("Deleting room ~p", [Id]),
            Reply = room_sup:delete_room(Id),
            delete_room(Id, State);
        false ->
            Reply = not_found,
            State
    end,
    {reply, Reply, NewState}.
