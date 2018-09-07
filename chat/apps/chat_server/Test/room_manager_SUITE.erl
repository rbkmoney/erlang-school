-module(room_manager_SUITE).

-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile([export_all]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPE EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export_type([groupName/0]).
-export_type([proplist/0]).
-export_type([config/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type groupName() :: atom().
-type proplist() :: [{atom(), term()}].
-type config() :: proplist().

%%%%%%%%%%%%%%%%%%%%%%%%% TEST INITIALIZATION %%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() ->
    [{group, groupName()}].

all() ->
    [
        {group, room_management}
    ].

-spec groups() ->
    [{groupName(), [sequence], [atom()]}].

groups() ->
    [
        {room_management, [sequence], [
            get_room_list,
            create_room,
            find_room,
            cant_create_existing_room,
            delete_room,
            cant_delete_nonexistent_room,
            cant_find_nonexistent_room
        ]}
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%% SUITE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init_per_suite(C :: config()) ->
    config().

init_per_suite(C) ->
    C1 = [{default_room, <<"room1">>},
          {created_room, <<"room2">>},
          {register, <<"{\"user\":\"Igor\",\"room\":\"room1\",\"message\":\"Hello\",\"event\":\"register\"}">>},
          {send_message, <<"{\"user\":\"Igor\",\"room\":\"room1\",\"message\":\"Hello\",\"event\":\"send_message\"}">>}
         ] ++ C,
    application:ensure_all_started(chat_server),
    application:start(chat_server),
    C1.

-spec end_per_suite(C :: config()) ->
    config().

end_per_suite(C) ->
    C.

%%%%%%%%%%%%%%%%%%%%%%%%% GROUP INITIALIZATION %%%%%%%%%%%%%%%%%%%%%%%%

-spec init_per_group(groupName(), C :: config()) ->
    config().

init_per_group(room_management, C) ->
    C.

-spec end_per_group(groupName() ,C :: config()) ->
    config().

end_per_group(room_management, C) ->
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%%% ROOM MANAGEMENT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_room_list(C :: config()) ->
    config().

get_room_list(C) ->
    Default = ?config(default_room, C),
    [Default] = room_manager:get_rooms(),
    C.

-spec create_room(C :: config()) ->
    config().

create_room(C) ->
    Created = ?config(created_room, C),
    ok = room_manager:create_room(Created),
    C.

-spec find_room(C :: config()) ->
    config().

find_room(C) ->
    Created = ?config(created_room, C),
    true = is_pid(room_manager:get_room(Created)),
    C.

-spec cant_create_existing_room(C :: config()) ->
    config().

cant_create_existing_room(C) ->
    Created = ?config(created_room, C),
    already_exists = room_manager:create_room(Created),
    C.

-spec delete_room(C :: config()) ->
    config().

delete_room(C) ->
    Created = ?config(created_room, C),
    ok = room_manager:delete_room(Created),
    C.

-spec cant_delete_nonexistent_room(C :: config()) ->
    config().

cant_delete_nonexistent_room(C) ->
    Created = ?config(created_room, C),
    not_found = room_manager:delete_room(Created),
    C.

-spec cant_find_nonexistent_room(C :: config()) ->
    config().

cant_find_nonexistent_room(C) ->
    Created = ?config(created_room, C),
    not_found = room_manager:get_room(Created),
    C.
