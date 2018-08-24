-module(room_manager).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(gen_server).

-export([init/1]).
-export([start_link/0]).
-export([handle_cast/2]).
-export([handle_call/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([add_room/2]).
-export([get_room/1]).
-export([get_rooms/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type state() :: #{atom() => pid()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_room(Id, PID) ->
    lager:notice("Room ~p wants to sign up as ~p", [PID, Id]),
    gen_server:cast(?MODULE, {create, Id, PID}),
    ok.

get_rooms() ->
    gen_server:call(?MODULE, {get_rooms}).

get_room(Id) ->
    gen_server:call(?MODULE, {get_room_pid, Id}).

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

room_exists(Id, State) ->
    case find_room(Id, State) of
        not_found ->
            false;
        _ ->
            true
    end.

find_room(Id, State) ->
    maps:get(Id, State, not_found).

add_room(Id, PID, State) ->
    maps:put(Id, PID, State).

room_id_list(State) ->
    maps:keys(State).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

init(undefined) ->
    lager:notice("Initialized room manager"),
    {ok, #{}}.

handle_cast({create, Id, PID}, State) ->
    case room_exists(Id, State) of
        false ->
            NewState = add_room(Id, PID, State),
            lager:info("Room ~p succesfully signed up in manager", [Id]);
        true ->
            lager:info("Room ~p already exists", [Id]),
            NewState = State
    end,
    {noreply, NewState}.

handle_call({get_rooms}, _From, State) ->
    Rooms = room_id_list(State),
    {reply, Rooms, State};

handle_call({get_room_pid, Id}, _From, State) ->
    RoomPID = find_room(Id, State),
    {reply, RoomPID, State};
handle_call(_, _, State) ->
    {ok, reply, State}.
