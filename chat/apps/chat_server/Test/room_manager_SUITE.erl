-module(room_manager_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

all() ->
    [
        {group, room_management}
    ].

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

init_per_suite(C) ->
    C1 = [{default_room, room1},
          {created_room, room2},
          {register, <<"{\"user\":\"Igor\",\"room\":\"room1\",\"message\":\"Hello\",\"event\":\"register\"}">>},
          {send_message, <<"{\"user\":\"Igor\",\"room\":\"room1\",\"message\":\"Hello\",\"event\":\"send_message\"}">>}
         ] ++ C,
    application:ensure_all_started(chat_server),
    application:start(chat_server),
    C1.

end_per_suite(C) ->
    C.

init_per_group(room_management, C) ->
    C1 = [{default_room, room1}, {created_room, room2}] ++ C,
    C1.

end_per_group(room_management, C) ->
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%%% ROOM MANAGEMENT %%%%%%%%%%%%%%%%%%%%%%%%%%%

get_room_list(C) ->
    Default = get(default_room, C),
    [Default] = room_manager:get_rooms(),
    ct:print("Room manager returned ~p", [Default]),
    C.

create_room(C) ->
    Created = get(created_room, C),
    created = room_manager:create_room(Created),
    C.

find_room(C) ->
    Created = get(created_room, C),
    PID = room_manager:get_room(Created),
    C.

cant_create_existing_room(C) ->
    Created = get(created_room, C),
    already_exists = room_manager:create_room(Created),
    C.

delete_room(C) ->
    Created = get(created_room, C),
    deleted = room_manager:delete_room(Created),
    C.

cant_delete_nonexistent_room(C) ->
    Created = get(created_room, C),
    not_found = room_manager:delete_room(Created),
    C.

cant_find_nonexistent_room(C) ->
    Created = get(created_room, C),
    not_found = room_manager:get_room(Created),
    C.

get(Key, List) ->
    proplists:get_value(Key, List).
