-module(chatserv_room_manager).
-behavior(gen_server).

%% API
-type rooms_by_id() :: #{chatlib_proto:room_id() => pid()}.
-type rooms_by_pid() :: #{pid() => chatlib_proto:room_id()}.


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
    RoomList = gen_server:call(?SERVER, get_rooms_list),

    maps:map(
        fun(_, V) ->
            chatserv_room:get_room_name(V)
        end,
        RoomList
    ).

-spec get_room_pid(chatlib_proto:room_id()) ->
    {error, chatlib_proto:response_code()} | {ok, pid()}.
get_room_pid(RoomId) ->
    gen_server:call(?SERVER, {get_room_pid, RoomId}).

%%
%% gen_server
%%
-type state() :: #{
    rooms := rooms_by_id(),
    rooms_by_pid := rooms_by_pid()
}.

-spec start_link() ->
    {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


-spec init([]) ->
    {ok, state(), {continue, load_rooms}}.
init([]) ->
    {ok, #{
        rooms => #{},
        rooms_by_pid => #{}
    }, {
        continue, load_rooms
    }}.

-spec handle_call(get_rooms_list, {pid(), _}, state()) ->
    {reply, rooms_by_id(), state()}.
handle_call(get_rooms_list, _, State = #{rooms := Rooms}) ->
    {reply, Rooms, State};

handle_call({get_room_pid, RoomId}, _, State = #{rooms := Rooms}) ->
    case room_exists(RoomId, Rooms) of
        true ->
            {reply, {ok, get_room_pid_by_id(RoomId, Rooms)}, State};

        false ->
            {reply, {error, room_does_not_exist}, State}
    end.

-spec handle_cast(any(), state()) ->
    {noreply, state()}.
handle_cast(_, State) ->
    {noreply, State}.

-spec handle_continue(load_rooms, state()) ->
    {noreply, state()}.
handle_continue(load_rooms, State) ->
    ResState = maps:fold(
        fun(Id, Name, CurState) ->
            add_room(Id, Name, CurState)
        end,
        State, #{1=>"Room", 2=>"Better room"}
    ),

    {noreply, ResState}.

-spec handle_info({'DOWN', reference(), process, pid(), atom()}, state()) ->
    {noreply, state()}.
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #{rooms := Rooms}) ->
    RoomId = get_room_id_by_pid(Pid, Rooms),
    NewState = remove_room(RoomId, State),
    {noreply, NewState}.

%%
%% Internal
%%

-spec room_exists(chatlib_proto:room_id(), rooms_by_id()) ->
    boolean().
room_exists(RoomId, Rooms) ->
    maps:is_key(RoomId, Rooms).

-spec add_room(chatlib_proto:room_id(), chatlib_proto:room_name(), Old :: state()) ->
    New :: state().
add_room(RoomId, RoomName, State = #{rooms := Rooms, rooms_by_pid := RoomsByPid}) ->
    case room_exists(RoomId, Rooms) of
        false ->
            {ok, Pid} = chatserv_room_sup:start_room(RoomId, RoomName),
            _ = erlang:monitor(process, Pid),

            State#{
                rooms := maps:put(RoomId, Pid, Rooms),
                rooms_by_pid := maps:put(Pid, RoomId, RoomsByPid)
            };

        true ->
            State
    end.

-spec remove_room(chatlib_proto:room_id(), Old :: state()) ->
    New :: state().
remove_room(RoomId, State = #{rooms := Rooms, rooms_by_pid := RoomsByPid}) ->
    Pid = get_room_pid_by_id(RoomId, Rooms),

    State#{
        rooms := maps:remove(RoomId, Rooms),
        rooms_by_pid := maps:remove(Pid, RoomsByPid)
    }.

-spec get_room_pid_by_id(chatlib_proto:room_id(), rooms_by_id()) ->
    pid().
get_room_pid_by_id(RoomId, RoomList) ->
    maps:get(RoomId, RoomList).

-spec get_room_id_by_pid(pid(), rooms_by_pid()) ->
    chatlib_proto:room_id().
get_room_id_by_pid(RoomPid, RoomList) ->
    maps:get(RoomPid, RoomList).
