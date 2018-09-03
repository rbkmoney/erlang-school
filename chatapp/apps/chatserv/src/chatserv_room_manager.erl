-module(chatserv_room_manager).
-behavior(gen_server).

%% API
-type room_ids() :: list(chatlib_proto:room_id_direct()).

-export([
    get_rooms_with_names/0,
    room_exists/1
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

    lists:foldl(
        fun(Id, Acc) ->
            Acc#{Id => chatserv_room:get_room_name(Id)}
        end,
        #{}, RoomList
    ).

room_exists(RoomId) ->
    gen_server:call(?SERVER, {room_exists, RoomId}).

%%
%% gen_server
%%
-type state() :: #{
    room_ids := room_ids()
}.

-spec start_link() ->
    {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec init([]) ->
    {ok, state(), {continue, load_rooms}}.
init([]) ->
    {ok, #{
        room_ids => []
    }, {
        continue, load_rooms
    }}.

-spec handle_call(get_rooms_list | {room_exists, chatlib_proto:room_id_direct()}, any(), state()) ->
    {reply, room_ids() | boolean(), state()}.
handle_call(get_rooms_list, _, State = #{room_ids := Rooms}) ->
    {reply, Rooms, State};

handle_call({room_exists, RoomId}, _, State = #{room_ids := Rooms}) ->
    {reply, do_room_exists(RoomId, Rooms), State}.

-spec handle_cast(any(), state()) ->
    {noreply, state()}.
handle_cast(_, State) ->
    {noreply, State}.

-spec handle_continue(load_rooms, state()) ->
    {noreply, state()}.
handle_continue(load_rooms, State) ->
    ResState = maps:fold(
        fun add_room/3,
        State, #{1=>"Room", 2=>"Better room"}
    ),

    {noreply, ResState}.

-spec handle_info({gproc, unreg, _, {n, l, {chat_room, chatlib_proto:room_id_direct()}}}, state()) ->
    {noreply, state()}.
handle_info({gproc, unreg, _, {n, l, {chat_room, RoomId}}}, State) ->
    NewState = remove_room(RoomId, State),
    {noreply, NewState}.

%%
%% Internal
%%

-spec do_room_exists(chatlib_proto:room_id_direct(), room_ids()) ->
    boolean().
do_room_exists(RoomId, Rooms) ->
    lists:member(RoomId, Rooms).

-spec add_room(chatlib_proto:room_id_direct(), chatlib_proto:room_name(), Old :: state()) ->
    New :: state().
add_room(RoomId, RoomName, State = #{room_ids := Rooms}) ->
    case do_room_exists(RoomId, Rooms) of
        false ->
            {ok, _} = chatserv_room_sup:start_room(RoomId, RoomName),
            _ = gproc:monitor({n, l, {chat_room, RoomId}}),

            State#{room_ids := [RoomId | Rooms]};

        true ->
            State
    end.

-spec remove_room(chatlib_proto:room_id_direct(), Old :: state()) ->
    New :: state().
remove_room(RoomId, State = #{room_ids := Rooms}) ->
    State#{room_ids := lists:delete(RoomId, Rooms)}.
