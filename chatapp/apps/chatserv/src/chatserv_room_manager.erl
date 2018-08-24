-module(chatserv_room_manager).
-behavior(gen_server).

%% API
-type room_list() :: #{chatlib_proto:room_id() => pid()}.


-export([
    get_rooms_with_names/0,
    get_room_pid/1
]).

%% gen_server
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_continue/2,
    handle_info/2
]).

-define(SERVER, ?MODULE).

%%
%% API
%%

-spec get_rooms_with_names() ->
    chatlib_proto:room_list().
get_rooms_with_names() ->
    RoomList = get_room_list(),

    maps:map(
        fun(_, V) ->
            chatserv_room:get_room_name(V)
        end,
        RoomList
    ).

-spec get_room_pid(chatlib_proto:room_id()) ->
    {error, chatlib_proto:response_code()} | {ok, pid()}.
get_room_pid(RoomId) ->
    RoomList = get_room_list(),

    case room_exists(RoomId, RoomList) of
        true ->
            {ok, get_room_pid_by_id(RoomId, RoomList)};

        false ->
            {error, room_does_not_exist}
    end.

-spec get_room_list() -> any().
get_room_list() ->
    gen_server:call(?SERVER, get_rooms_list).

%%
%% gen_server
%%
-type state() :: #{
rooms := room_list()
}.

-spec start_link() ->
    {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


-spec init([]) ->
    {ok, state(), {continue, load_rooms}}.
init([]) ->
    {ok, #{
        rooms => #{}
    }, {
        continue, load_rooms
    }}.

-spec handle_call(get_rooms_list, {pid(), _}, state()) ->
    {reply, room_list(), state()}.
handle_call(get_rooms_list, _, State = #{rooms := Rooms}) ->
    {reply, Rooms, State}.

-spec handle_cast(any(), state()) ->
    {noreply, state()}.
handle_cast(_, State) ->
    {noreply, State}.

-spec handle_continue(load_rooms, state()) ->
    {noreply, state()}.
handle_continue(load_rooms, State) ->
    NewRooms = maps:fold(
        fun(Id, Name, Rooms) ->
            {ok, RoomList} = add_room(Id, Name, Rooms),

            RoomList
        end,
        #{}, #{1=>"Room", 2=>"Better room"}
    ),

    {noreply, State#{rooms := NewRooms}}.

-spec handle_info({'DOWN', reference(), process, pid(), atom()}, state()) ->
    {noreply, state()}.
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #{rooms := Rooms}) ->
    RoomId = get_room_id_by_pid(Pid, Rooms),
    NewList = remove_room(RoomId, Rooms),

    {noreply, State#{rooms := NewList}}.

%%
%% Internal
%%

-spec room_exists(chatlib_proto:room_id(), room_list()) ->
    boolean().
room_exists(RoomId, Rooms) ->
    maps:is_key(RoomId, Rooms).

-spec add_room(chatlib_proto:room_id(), chatlib_proto:room_name(), Old :: room_list()) ->
    {ok, New :: room_list()} | {error, room_already_exists}.
add_room(RoomId, RoomName, Rooms) ->
    case room_exists(RoomId, Rooms) of
        true ->
            {error, room_already_exists};
        false ->
            {ok, Pid} = supervisor:start_child(chsv_room_sup, [RoomId, RoomName]),
            _ = erlang:monitor(process, Pid),

            {ok, maps:put(RoomId, Pid, Rooms)}
    end.

-spec remove_room(chatlib_proto:room_id(), Old :: room_list()) ->
    New :: room_list().
remove_room(RoomId, Rooms) ->
    maps:remove(RoomId, Rooms).

-spec get_room_pid_by_id(chatlib_proto:room_id(), room_list()) ->
    pid().
get_room_pid_by_id(RoomId, RoomList) ->
    maps:get(RoomId, RoomList).

%@todo kinda bad
get_room_id_by_pid(RoomPid, RoomList) ->
    ResMap = maps:filter(
        fun(_, V) ->
            V == RoomPid
        end,
        RoomList
    ),
    [Res] = maps:values(ResMap),

    Res.

