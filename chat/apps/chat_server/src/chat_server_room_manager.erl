-module(chat_server_room_manager).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(gen_server).

-export([init       /1]).
-export([handle_cast/2]).
-export([handle_call/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([rooms     /0]).
-export([start_link/0]).
-export([create_room/1]).
-export([delete_room/1]).
-export([room_exists/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type room_list() :: [library_protocol:room()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() ->
    {ok, pid()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

-spec room_exists(RoomId :: library_protocol:room()) ->
    boolean().

room_exists(RoomId) ->
    gen_server:call(?MODULE, {room_exists, RoomId}).

-spec create_room(RoomId :: library_protocol:room()) ->
    ok | already_exists.

create_room(RoomId) ->
    gen_server:call(?MODULE, {create_room, RoomId}).

-spec delete_room(RoomId :: library_protocol:room()) ->
    ok | not_found.

delete_room(RoomId) ->
    gen_server:call(?MODULE, {delete_room, RoomId}).

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
    ({create_room, RoomId :: library_protocol:room()}, _From :: term(), RoomList :: room_list()) ->
        {reply, ok | already_exists, room_list()};
    ({room_exists, RoomId :: library_protocol:room()}, _From :: term(), RoomList :: room_list()) ->
        {reply, boolean(), room_list()};
    ({delete_room, RoomId :: library_protocol:room()}, _From :: term(), RoomList :: room_list()) ->
        {reply, ok | not_found, room_list()};
    (rooms, _From :: term(), RoomList :: room_list()) ->
        {reply, room_list(), room_list()}.


handle_call({create_room, RoomId}, _From, RoomList) ->
    {Reply, NewRoomList} = case lists:member(RoomId, RoomList) of
        false ->
            ok = lager:info("Creating room ~p", [RoomId]),
            {ok, [RoomId | RoomList]};
        true ->
            ok = lager:info("Can't create room ~p, already_exists", [RoomId]),
            {already_exists, RoomList}
    end,
    {reply, Reply, NewRoomList};

handle_call({room_exists, RoomId}, _From, RoomList) ->
    ok = lager:info("Checking if room ~p exists", [RoomId]),
    Reply = lists:member(RoomId, RoomList),
    {reply, Reply, RoomList};

handle_call({delete_room, RoomId}, _From, RoomList) ->
    {Reply, NewRoomList} = case lists:member(RoomId, RoomList) of
        true ->
            ok = lager:info("Deleting room ~p", [RoomId]),
            {ok, lists:delete(RoomId, RoomList)};
        false ->
            ok = lager:info("Can't delete room ~p, not_found", [RoomId]),
            {not_found, RoomList}
    end,
    {reply, Reply, NewRoomList};

handle_call(rooms, _From, RoomList) ->
    {reply, RoomList, RoomList}.
