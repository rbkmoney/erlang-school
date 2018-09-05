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

-type state() :: #{atom() => pid()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec create_room(Id :: atom()) ->
    created | already_exists.

create_room(Id) ->
    case get_room(Id) of
        not_found ->
            created = room_sup:create_room(Id),
            created;
        _ ->
            already_exists
    end.

-spec register_room(Id :: atom(), PID :: pid()) ->
    ok.

register_room(Id, PID) ->
    ok = lager:notice("Room ~p wants to sign up as ~p", [PID, Id]),
    gen_server:cast({global, ?MODULE}, {create, Id, PID}),
    ok.

-spec get_room(Id :: term) ->
    pid() | not_found.

get_room(Id) ->
    gen_server:call({global, ?MODULE}, {get_room_pid, Id}).

-spec get_rooms() ->
    [atom()].

get_rooms() ->
    gen_server:call({global, ?MODULE}, {get_rooms}).

-spec delete_room(Id :: atom) ->
    deleted | not_found.

delete_room(Id) ->
    gen_server:call({global, ?MODULE}, {delete_room, Id}).

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec room_exists(Id :: atom(), State :: state()) ->
    boolean().

room_exists(Id, State) ->
    case find_room(Id, State) of
        not_found ->
            false;
        _ ->
            true
    end.

-spec find_room(Id :: atom(), State :: state()) ->
    pid() | not_found.

find_room(Id, State) ->
    maps:get(Id, State, not_found).

-spec register_room(Id :: atom(), PID :: pid(), State :: state()) ->
    state().

register_room(Id, PID, State) ->
    maps:put(Id, PID, State).

-spec delete_room(Id :: atom(), State :: state()) ->
    state().

delete_room(Id, State) ->
    maps:remove(Id, State).

-spec room_id_list(State :: state()) ->
    [atom()].

room_id_list(State) ->
    maps:keys(State).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() ->
    {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, undefined, []).

-spec init(undefined) ->
    {ok, state()}.

init(undefined) ->
    ok = lager:notice("Initialized room manager"),
    {ok, #{}}.

-spec handle_cast
    ({create, Id :: atom(), PID :: pid()}, State :: state()) ->
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
    ({get_rooms}, _From :: {pid(), reference()}, State :: state()) -> %Tag is taken from docs
        {reply, list(), state()};
    ({get_room_pid, Id :: atom()}, _From :: {pid(), reference()}, State :: state()) ->
        {reply, pid(), state()};
    ({delete_room, Id :: atom()}, _From :: {pid(), reference()}, State :: state()) ->
        {reply, deleted | not_found, state()}.


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
            deleted = room_sup:delete_room(Id),
            Reply = deleted,
            delete_room(Id, State);
        false ->
            Reply = not_found,
            State
    end,
    {reply, Reply, NewState}.
