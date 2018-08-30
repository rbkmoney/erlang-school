-module(library_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

all() ->
    [
        {group, basic_interactions}
    ].

groups() ->
    [
        {basic_interactions, [sequence], [
            encodeJson,
            decodeJson,
            reverse
        ]}
    ].

init_per_suite(C) ->
    C1 = [{event, send_message},
            {user, <<"Igor">>},
            {message, <<"Hello">>},
            {room, room1},
            {encoded, <<"{\"user\":\"Igor\",\"room\":\"room1\",\"message\":\"Hello\",\"event\":\"send_message\"}">>},
            {decoded, {send_message, <<"Igor">>, <<"Hello">>, room1}}
         ] ++ C,
    application:ensure_all_started(library),
    application:start(library),
    C1.

end_per_suite(C) ->
    application:stop(library),
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%% BASIC INTERACTIONS %%%%%%%%%%%%%%%%%%%%%%%%%

encodeJson(C) ->
    Json = get(encoded, C),
    Event = get(event, C),
    User = get(user, C),
    Message = get(message, C),
    Room = get(room, C),
    Json = protocol:encode(Event, User, Message, Room),
    C.

decodeJson(C) ->
    Target = get(decoded, C),
    Json = get(encoded, C),
    Target = protocol:decode(Json),
    C.

reverse(C) ->
    Event = get(event, C),
    User = get(user, C),
    Message = get(message, C),
    Room = get(room, C),
    Json = protocol:encode(Event, User, Message, Room),
    {Event, User, Message, Room} = protocol:decode(Json),
    C.

get(Key, List) ->
    proplists:get_value(Key, List).
