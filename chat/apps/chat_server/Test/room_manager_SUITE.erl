-module(room_manager_SUITE).

-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile([export_all]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MACROSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(FIRST_ROOM, <<"room1">>).
-define(SECOND_ROOM, <<"room2">>).

%%%%%%%%%%%%%%%%%%%%%%%%% TEST INITIALIZATION %%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() ->
    [atom()].

all() ->
    [
    create_room,
    get_room_list,
    get_room_pid,
    cant_create_existing_room,
    delete_room,
    cant_delete_nonexistent_room,
    cant_find_nonexistent_room
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%% SUITE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init_per_suite(C :: config()) ->
    config().

init_per_suite(C) ->
    {ok, Apps} = application:ensure_all_started(chat_server),
    [{apps, [Apps]} | C].

-spec end_per_suite(C :: config()) ->
    term().

end_per_suite(C) ->
    [application:stop(App) || App <- ?config(apps, C)].

%%%%%%%%%%%%%%%%%%%%%%% ROOM MANAGEMENT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec create_room(C :: config()) ->
    term().

create_room(_C) ->
    ok = room_manager:create_room(?FIRST_ROOM).

-spec get_room_list(C :: config()) ->
    term().

get_room_list(_C) ->
    [?FIRST_ROOM] = room_manager:rooms().

-spec get_room_pid(C :: config()) ->
    term().

get_room_pid(_C) ->
    true = is_pid(room_manager:room_pid(?FIRST_ROOM)).

-spec cant_create_existing_room(C :: config()) ->
    term().

cant_create_existing_room(_C) ->
    already_exists = room_manager:create_room(?FIRST_ROOM).

-spec delete_room(C :: config()) ->
    term().

delete_room(_C) ->
    ok = room_manager:delete_room(?FIRST_ROOM).

-spec cant_delete_nonexistent_room(C :: config()) ->
    term().

cant_delete_nonexistent_room(_C) ->
    not_found = room_manager:delete_room(?SECOND_ROOM).

-spec cant_find_nonexistent_room(C :: config()) ->
    term().

cant_find_nonexistent_room(_C) ->
    false = room_manager:room_exists(?SECOND_ROOM).
