-module(room_manager_SUITE).

-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% tests descriptions

-export([all           /0]).
-export([init_per_suite/1]).
-export([end_per_suite /1]).

%% tests

-export([create_room                 /1]).
-export([delete_room                 /1]).
-export([get_room_list               /1]).
-export([cant_create_existing_room   /1]).
-export([cant_find_nonexistent_room  /1]).
-export([cant_delete_nonexistent_room/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MACROSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(FIRST_ROOM,  <<"room1">>).
-define(SECOND_ROOM, <<"room2">>).

%%%%%%%%%%%%%%%%%%%%%%%%% TEST INITIALIZATION %%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() ->
    [atom()].

all() ->
    [
        create_room,
        get_room_list,
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
    ok = chat_server_room_manager:create_room(?FIRST_ROOM).

-spec get_room_list(C :: config()) ->
    term().

get_room_list(_C) ->
    [?FIRST_ROOM] = chat_server_room_manager:rooms().

-spec cant_create_existing_room(C :: config()) ->
    term().

cant_create_existing_room(_C) ->
    already_exists = chat_server_room_manager:create_room(?FIRST_ROOM).

-spec delete_room(C :: config()) ->
    term().

delete_room(_C) ->
    ok = chat_server_room_manager:delete_room(?FIRST_ROOM).

-spec cant_delete_nonexistent_room(C :: config()) ->
    term().

cant_delete_nonexistent_room(_C) ->
    not_found = chat_server_room_manager:delete_room(?SECOND_ROOM).

-spec cant_find_nonexistent_room(C :: config()) ->
    term().

cant_find_nonexistent_room(_C) ->
    false = chat_server_room_manager:room_exists(?SECOND_ROOM).
