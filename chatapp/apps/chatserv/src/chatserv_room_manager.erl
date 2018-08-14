-module(chatserv_room_manager).
-behavior(gen_server).

%%API
-type room_manager_state() :: #{
    rooms := [{non_neg_integer(), pid()}]
}.

%% gen_server
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_continue/2,
    handle_info/2
]).


%%
%% gen_server
%%

-spec start_link() ->
    {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, room_manager}, ?MODULE, [], []).

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
    {noreply, State#{rooms:=NewRooms}}.

%%@todo handle room crashes
-spec handle_info({'DOWN', reference(), process, pid(), atom()}, room_manager_state()) ->
    {noreply, room_manager_state()}.
handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    {noreply, State}.

%%
%% Internal
%%