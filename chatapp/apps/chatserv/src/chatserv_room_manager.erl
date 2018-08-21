-module(chatserv_room_manager).
-behavior(gen_server).

%% API
-type room_manager_state() :: #{
    rooms := #{ chatlib_proto:room_id() => pid() }
}.

-export([
    get_rooms_list/0,
    get_room/1
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

%@todo replace string type with another proto type
-spec get_rooms_list() ->
    #{chatlib_proto:room_id() => chatlib_proto:room_name()}.
get_rooms_list() ->
    RoomList = gen_server:call(?SERVER, get_rooms_list),

    maps:map(
        fun(_, V) ->
            chatserv_room:get_name_of(V)
        end,
        RoomList
    ).

-spec get_room(chatlib_proto:room_id()) ->
    pid().
get_room(RoomId) ->
    RoomList = gen_server:call(?SERVER, get_rooms_list),

    maps:get(RoomId, RoomList).

%%
%% gen_server
%%

-spec start_link() ->
    {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #{
        rooms => []
    }, {
        continue, load_rooms
    }}.

handle_call(get_rooms_list, _, State = #{rooms := Rooms}) ->
    {reply, Rooms, State}.

handle_cast(_, State) ->
    {noreply, State}.

-spec handle_continue(load_rooms, room_manager_state()) ->
    {noreply, room_manager_state()}.
handle_continue(load_rooms, State) ->
    NewRooms = lists:map(
        fun(I) ->
            {ok, Pid} = supervisor:start_child(chsv_room_sup, [I, "Test Room"]),
            erlang:monitor(process, Pid),
            {I, Pid}
        end,
        lists:seq(0, 1)
    ),
    {noreply, State#{ rooms := maps:from_list(NewRooms) }}.

%%@todo handle room crashes
-spec handle_info({'DOWN', reference(), process, pid(), atom()}, room_manager_state()) ->
    {noreply, room_manager_state()}.
handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    {noreply, State}.

%%
%% Internal
%%