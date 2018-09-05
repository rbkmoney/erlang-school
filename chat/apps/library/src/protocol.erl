-module(protocol).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([room/1]).
-export([user/1]).
-export([event/1]).
-export([encode/1]).
-export([encode/4]).
-export([decode/1]).
-export([message/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPE EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export_type([json/0]).
-export_type([source_message/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type json() :: binary().
-type source_message() :: {atom(), binary(), binary(), atom()}.
-type message_map() :: #{
    event => atom(),
    user => binary(),
    message => binary(),
    room => atom()}.
-type binary_key_map() :: #{<<>> => <<>>}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec encode(source_message()) ->
    json().

encode({Event, User, Message, RoomId}) ->
    encode(Event, User, Message, RoomId).

-spec encode(Event :: atom(), User :: binary(), Message :: binary(), RoomId :: atom()) ->
    json().

encode(Event, User, Message, RoomId) ->
    Map = raw_to_map(Event, User, Message, RoomId),
    create_json(Map).

-spec decode(json()) ->
    source_message().

decode(Json) ->
    Binary_key_map = jiffy:decode(Json, [return_maps]),
    normalize_map(Binary_key_map).

-spec room(source_message()) ->
    atom().

room({_Event, _Username, _Message, RoomId}) ->
    RoomId.

-spec event(source_message()) ->
    atom().

event({Event, _Username, _Message, _RoomId}) ->
    Event.

-spec message(source_message()) ->
    binary().

message({_Event, _Username, Message, _RoomId}) ->
    Message.

-spec user(source_message()) ->
    binary().

user({_Event, Username, _Message, _RoomId}) ->
    Username.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec raw_to_map(Event :: atom(), User :: binary(), Message :: binary(), RoomId :: atom()) ->
    message_map().

raw_to_map(Event, User, Message, RoomId) ->
    #{event => Event, user => User, message => Message, room => RoomId}.

-spec create_json(message_map()) ->
    json().

create_json(Map) ->
    jiffy:encode(Map).

-spec normalize_map(binary_key_map()) ->
    source_message().

normalize_map(Binary_key_map) -> % Not really nessesary, yet added to extend the readability
    Event = binary_to_atom(maps:get(<<"event">>, Binary_key_map)),
    User = maps:get(<<"user">>, Binary_key_map),
    Message = maps:get(<<"message">>, Binary_key_map),
    RoomId = binary_to_atom(maps:get(<<"room">>, Binary_key_map)),
    {Event, User, Message, RoomId}.

-spec binary_to_atom(Binary :: binary()) ->
    atom().

binary_to_atom(Binary) ->
    List = binary_to_list(Binary),
    list_to_atom(List).
