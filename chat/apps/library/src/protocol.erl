-module(protocol).


% На вход подаем {atom, binary, binary, binary}
% На выходе получаем binary
% На вход подаем binary
% На выходе получаем {atom, binary, binary, binary}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([room/1]).
-export([user/1]).
-export([event/1]).
-export([encode/1]).
-export([decode/1]).
-export([message/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPE EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export_type([source_message/0]).
-export_type([event/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type event() :: register | left | joined | error | send_message | success.
-type source_message() :: {event(), binary(), binary(), binary()}.
-type binary_key_map() :: #{binary() => binary() | atom()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec encode(source_message()) ->
    jiffy:json_value().

encode(SourceMessage) ->
    Map = raw_to_map(SourceMessage),
    jiffy:encode(Map).

-spec decode(jiffy:json_value()) ->
    source_message().

decode(Json) ->
    Map = jiffy:decode(Json, [return_maps]),
    map_to_raw(Map).

-spec room(source_message()) ->
    binary().

room({_Event, _Username, _Message, Room}) ->
    Room.

-spec event(source_message()) ->
    atom().

event({Event, _Username, _Message, _Room}) ->
    Event.

-spec message(source_message()) ->
    binary().

message({_Event, _Username, Message, _Room}) ->
    Message.

-spec user(source_message()) ->
    binary().

user({_Event, Username, _Message, _Room}) ->
    Username.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec raw_to_map(source_message()) ->
    binary_key_map().

raw_to_map({Event, User, Message, Room}) ->
    #{<<"event">> => Event, <<"user">> => User, <<"message">> => Message, <<"room">> => Room}.

-spec map_to_raw(binary_key_map()) ->
    source_message().

map_to_raw(#{<<"event">> := Event, <<"user">> := User, <<"message">> := Msg, <<"room">> := Room}) ->
    {decode_event(Event), User, Msg, Room}.

-spec decode_event(binary()) ->
    event().

decode_event(<<"send_message">>) ->
    send_message;
decode_event(<<"register">>) ->
    register;
decode_event(<<"joined">>) ->
    joined;
decode_event(<<"error">>) ->
    error;
decode_event(<<"left">>) ->
    left;
decode_event(<<"success">>) ->
    success;
decode_event(_) ->
    undefined.
