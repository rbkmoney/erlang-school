-module(library_SUITE).

-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile([export_all]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type groupName() :: room_manager_SUITE:groupName().
-type proplist() :: room_manager_SUITE:proplist().
-type config() :: room_manager_SUITE:config().

%%%%%%%%%%%%%%%%%%%%%%%%% TEST INITIALIZATION %%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() ->
    [{group, groupName()}].

all() ->
    [
        {group, basic_interactions}
    ].

-spec groups() ->
    [{groupName(), [sequence], [atom()]}].

groups() ->
    [
        {basic_interactions, [sequence], [
            encodeJson,
            decodeJson,
            reverse
        ]}
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%% SUITE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init_per_suite(C :: config()) ->
    config().

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

-spec end_per_suite(C :: config()) ->
    config().

end_per_suite(C) ->
    application:stop(library),
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%% BASIC INTERACTIONS %%%%%%%%%%%%%%%%%%%%%%%%%

-spec encodeJson(C :: config()) ->
    config().

encodeJson(C) ->
    Json = get(encoded, C),
    Event = get(event, C),
    User = get(user, C),
    Message = get(message, C),
    Room = get(room, C),
    Json = protocol:encode(Event, User, Message, Room),
    C.

-spec decodeJson(C :: config()) ->
    config().

decodeJson(C) ->
    Target = get(decoded, C),
    Json = get(encoded, C),
    Target = protocol:decode(Json),
    C.

-spec reverse(C :: config()) ->
    config().

reverse(C) ->
    Event = get(event, C),
    User = get(user, C),
    Message = get(message, C),
    Room = get(room, C),
    Json = protocol:encode(Event, User, Message, Room),
    {Event, User, Message, Room} = protocol:decode(Json),
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get(Key :: atom(), List :: proplist()) ->
    term() | undefined.

get(Key, List) ->
    proplists:get_value(Key, List).
