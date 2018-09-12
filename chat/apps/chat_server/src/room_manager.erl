-module(room_manager).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(gen_server).
-export([init/1]).
-export([handle_cast/2]).
-export([handle_call/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([room_exists/1]).
-export([get_rooms/0]).
-export([start_link/0]).
-export([create_room/1]).
-export([delete_room/1]).
-export([register_room/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type state() :: [binary()]. % RoomName => it's pid

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() ->
    {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

-spec create_room(Id :: binary()) ->
    ok | already_exists.


room_exists(Id) ->
    gen_server:call(?MODULE, {exists, Id}).

create_room(Id) ->
    case room_exists(Id) of
        false ->
            chat_room:start_link(Id),
            ok;
        true ->
            already_exists
    end.

-spec register_room(Id :: binary()) ->
    ok.

register_room(Id) ->
    ok = lager:notice("Room wants to sign up as ~p", [Id]),
    gen_server:call(?MODULE, {create, Id}).

-spec get_rooms() ->
    [binary()].

get_rooms() ->
    gen_server:call(?MODULE, get_rooms).

-spec delete_room(Id :: binary()) ->
    deleted | not_found.

delete_room(Id) ->
    gen_server:call(?MODULE, {delete_room, Id}).

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec room_exists(Id :: binary(), State :: state()) ->
    boolean().

room_exists(Id, State) ->
    lists:member(Id, State).

-spec register_room(Id :: binary(), State :: state()) ->
    state().

register_room(Id, State) ->
    [Id | State].

-spec delete_room(Id :: binary(), State :: state()) ->
    state().

delete_room(Id, State) ->
    lists:delete(Id, State).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(undefined) ->
    {ok, state()}.

init(undefined) ->
    ok = lager:notice("Initialized room manager"),
    {ok, []}.

-spec handle_call
    (get_rooms, _From :: {pid(), reference()}, State :: state()) -> % Tag is taken from docs
        {reply, [binary()], state()};
    ({delete_room, Id :: binary()}, _From :: {pid(), reference()}, State :: state()) ->
        {reply, ok | {error, room_sup:error()} | not_found, state()}.

handle_call({create, Id}, _From, State) ->
    case room_exists(Id, State) of
        false ->
            NewState = register_room(Id, State),
            Reply = ok,
            ok = lager:info("Room ~p succesfully signed up in manager", [Id]);
        true ->
            Reply = already_exists,
            ok = lager:info("Room ~p already exists", [Id]),
            NewState = State
    end,
    {reply, Reply, NewState};

handle_call(get_rooms, _From, State) ->
    {reply, State, State};

handle_call({delete_room, Id}, _From, State) ->
    NewState = case room_exists(Id, State) of
        true ->
            ok = lager:notice("Deleting room ~p", [Id]),
            PID = gproc:lookup_local_name({chat_room, Id}),
            PID ! {stop, normal},
            true = gproc:unreg_other({n, l, {chat_room, Id}}, PID),
            Reply = ok,
            delete_room(Id, State);
        false ->
            Reply = not_found,
            State
    end,
    {reply, Reply, NewState};

handle_call({exists, Id}, _From, State) ->
    Reply = room_exists(Id, State),
    {reply, Reply, State}.

handle_cast(_, State) ->
    {noreply, State}.
