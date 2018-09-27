-module(chat_server_room_manager).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type room_list() :: [binary()].

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
    room_list().

rooms() ->
    gen_server:call(?MODULE, rooms).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(undefined) ->
    {ok, []}.

init(undefined) ->
    ok = lager:notice("Room manager initialized"),
    {ok, []}.

-spec handle_cast(term(), RoomList :: room_list()) ->
    {noreply, room_list()}.

handle_cast(_, RoomList) ->
    {noreply, RoomList}.

-spec handle_call
    ({create_room, Id :: binary}, _From :: term(), RoomList :: room_list()) ->
        {reply, ok | already_exists, room_list()};
    ({room_exists, Id :: binary}, _From :: term(), RoomList :: room_list()) ->
        {reply, boolean(), room_list()};
    ({delete_room, Id :: binary}, _From :: term(), RoomList :: room_list()) ->
        {reply, ok | not_found, room_list()};
    (rooms, _From :: term(), RoomList :: room_list()) ->
        {reply, room_list(), room_list()}.


handle_call({create_room, Id}, _From, RoomList) ->
    {Reply, NewRoomList} = case lists:member(Id, RoomList) of
        false ->
            ok = lager:info("Creating room ~p", [Id]),
            {ok, [Id | RoomList]};
        true ->
            ok = lager:info("Can't create room ~p, already_exists", [Id]),
            {already_exists, RoomList}
    end,
    {reply, Reply, NewRoomList};

handle_call({room_exists, Id}, _From, RoomList) ->
    ok = lager:info("Checking if room ~p exists", [Id]),
    Reply = lists:member(Id, RoomList),
    {reply, Reply, RoomList};

handle_call({delete_room, Id}, _From, RoomList) ->
    {Reply, NewRoomList} = case lists:member(Id, RoomList) of
        true ->
            ok = lager:info("Deleting room ~p", [Id]),
            {ok, lists:delete(Id, RoomList)};
        false ->
            ok = lager:info("Can't delete room ~p, not_found", [Id]),
            {not_found, RoomList}
    end,
    {reply, Reply, NewRoomList};

handle_call(rooms, _From, RoomList) ->
    {reply, RoomList, RoomList}.
