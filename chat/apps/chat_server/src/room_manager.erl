-module(room_manager).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(gen_server).

-export([init/1]).
-export([handle_cast/2]).
-export([handle_call/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([rooms/0]).
-export([start_link/0]).
-export([create_room/1]).
-export([delete_room/1]).
-export([room_exists/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() ->
    {ok, pid()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

-spec room_exists(Id :: binary()) ->
    boolean().

room_exists(Id) ->
    gen_server:call(?MODULE, {room_exists, Id}).

-spec create_room(Id :: binary()) ->
    ok | already_exists.

create_room(Id) ->
    gen_server:call(?MODULE, {create_room, Id}).

-spec delete_room(Id :: binary()) ->
    ok | not_found.

delete_room(Id) ->
    gen_server:call(?MODULE, {delete_room, Id}).

-spec rooms() ->
    [binary()].

rooms() ->
    gen_server:call(?MODULE, rooms).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(undefined) ->
    {ok, []}.

init(undefined) ->
    ok = lager:notice("Room manager initialized"),
    {ok, []}.

-spec handle_cast(term(), State :: stateless) ->
    {noreply, list()}.

handle_cast(_, State) ->
    {noreply, State}.

handle_call({create_room, Id}, _From, RoomList) ->
    Reply = case is_in_list(Id, RoomList) of
        false ->
            ok = lager:info("Creating room ~p", [Id]),
            NewRoomList = [Id | RoomList],
            ok;
        true ->
            ok = lager:info("Can't create room ~p, already_exists", [Id]),
            NewRoomList = RoomList,
            already_exists
    end,
    {reply, Reply, NewRoomList};

handle_call({room_exists, Id}, _From, RoomList) ->
    ok = lager:info("Checking if room ~p exists", [Id]),
    Reply = is_in_list(Id, RoomList),
    {reply, Reply, RoomList};

handle_call({delete_room, Id}, _From, RoomList) ->
    Reply = case is_in_list(Id, RoomList) of
        true ->
            ok = lager:info("Deleting room ~p", [Id]),
            NewRoomList = lists:delete(Id, RoomList),
            ok;
        false ->
            ok = lager:info("Can't delete room ~p, not_found", [Id]),
            NewRoomList = RoomList,
            not_found
    end,
    {reply, Reply, NewRoomList};

handle_call(rooms, _From, RoomList) ->
    {reply, RoomList, RoomList}.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

is_in_list(Item, List) -> % Возможно вынести в library
    ok = lager:info("Room manager searching for ~p in ~p", [Item, List]),
    case [Elem || Elem <- List, Elem == Item] of
        [] ->
            false;
        _ ->
            true
    end.
