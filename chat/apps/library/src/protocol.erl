-module(protocol).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([room/1]).
-export([user/1]).
-export([event/1]).
-export([encode/1]).
-export([encode/4]).
-export([decode/1]).
-export([message/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode({Event, User, Message, RoomId}) ->
    encode(Event, User, Message, RoomId).

encode(Event, User, Message, RoomId) ->
    Map = raw_to_map(Event, User, Message, RoomId),
    create_json(Map).

decode(Json) ->
    Binary_key_map = jiffy:decode(Json, [return_maps]),
    Map = normalize_map(Binary_key_map),
    map_to_raw(Map).

room({_Event, _Username, _Message, RoomId}) ->
    RoomId.

event({Event, _Username, _Message, _RoomId}) ->
    Event.

message({_Event, _Username, Message, _RoomId}) ->
    Message.

user({_Event, Username, _Message, _RoomId}) ->
    Username.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

raw_to_map(Event, User, Message, RoomId) ->
    #{event => Event, user => User, message => Message, room => RoomId}.

map_to_raw(Map) ->
    Event = binary_to_atom(maps:get(event, Map)),
    User = maps:get(user, Map),
    Message = maps:get(message, Map),
    RoomId = binary_to_atom(maps:get(room, Map)),
    {Event, User, Message, RoomId}.

create_json(Map) ->
    jiffy:encode(Map).

normalize_map(Binary_key_map) -> % Not really nessesary, yet added to extend the readability
    Event = maps:get(<<"event">>, Binary_key_map),
    User = maps:get(<<"user">>, Binary_key_map),
    Message = maps:get(<<"message">>, Binary_key_map),
    RoomId = maps:get(<<"room">>, Binary_key_map),
    raw_to_map(Event, User, Message, RoomId).

binary_to_atom(Binary) ->
    list_to_atom(binary_to_list(Binary)).
