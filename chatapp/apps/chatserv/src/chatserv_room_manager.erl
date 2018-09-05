-module(chatserv_room_manager).
-behavior(gen_server).

%% API

-export([
    get_rooms_with_names/0,
    check_room_exists/1
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
    gen_server:call(?SERVER, get_rooms_list).

-spec check_room_exists(chatlib_proto:room_id_direct()) ->
    boolean().
check_room_exists(RoomId) ->
    gen_server:call(?SERVER, {room_exists, RoomId}).

%%
%% gen_server
%%
-type state() :: #{
    rooms := chatlib_proto:room_list()
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


-spec handle_call(get_rooms_list | {room_exists, chatlib_proto:room_id_direct()}, _, state()) ->
    {reply, chatlib_proto:room_list() | boolean(), state()}.
handle_call(get_rooms_list, _, State = #{rooms := Rooms}) ->
    {reply, Rooms, State};

handle_call({room_exists, RoomId}, _, State = #{rooms := Rooms}) ->
    {reply, room_exists(RoomId, Rooms), State}.


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
        State, #{ 1 => "Room", 2 => "Better room"}
    ),

    {noreply, ResState}.

-spec handle_info(any(), state()) ->
    {noreply, state()}.
handle_info(_, State) ->
    {noreply, State}.

%%
%% Internal
%%

-spec room_exists(chatlib_proto:room_id(), chatlib_proto:room_list()) ->
    boolean().
room_exists(RoomId, Rooms) ->
    maps:is_key(RoomId, Rooms).

-spec add_room(chatlib_proto:room_id(), chatlib_proto:room_name(), Old :: state()) ->
    New :: state().
add_room(RoomId, RoomName, State = #{ rooms := Rooms }) ->
    case room_exists(RoomId, Rooms) of
        false ->
            State#{
                rooms := maps:put(RoomId, RoomName, Rooms)
            };
        true ->
            State
    end.
