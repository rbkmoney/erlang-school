-module(library_protocol).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([encode/1]).
-export([decode/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPE EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export_type([event  /0]).
-export_type([error  /0]).
-export_type([text   /0]).
-export_type([room   /0]).
-export_type([user   /0]).
-export_type([message/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type text() :: binary().
-type room() :: binary().
-type user() :: binary().
-type event() :: join | leave | create | delete | {message, text()}.
-type success_message() :: {event(), user(), room()}.
-type error_reason() :: already_exists | not_exists | already_joined | not_joined.
-type error() :: {error, error_reason()}.
-type message() :: success_message() | error().
-type mapped_json() :: #{binary() => binary()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec encode(Message :: message()) ->
    jiffy:json_value().

encode(Message) ->
    Map = to_map(Message),
    jiffy:encode(Map).

-spec decode(Json :: jiffy:json_value()) ->
    message().

decode(Json) ->
    Map = jiffy:decode(Json, [return_maps]),
    from_map(Map).

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec to_map(message()) ->
    mapped_json().

to_map({{message, Text}, User, Room}) ->
    #{<<"event">> => encode_event(message), <<"text">> => Text, <<"user">> => User, <<"room">> => Room};

to_map({error, Reason}) ->
    #{<<"event">> => encode_event(error), <<"text">> => encode_reason(Reason), <<"user">> => <<>>, <<"room">> => <<>>};

to_map({Event, User, Room}) ->
    #{<<"event">> => encode_event(Event), <<"text">> => <<>>, <<"user">> => User, <<"room">> => Room}.

-spec from_map(mapped_json()) ->
    message().

from_map(#{<<"event">> := <<"error">>, <<"text">> := Reason}) ->
    {error, decode_reason(Reason)};

from_map(#{<<"event">> := <<"message">>, <<"user">> := User, <<"text">> := Text, <<"room">> := Room}) ->
    {{message, Text}, User, Room};

from_map(#{<<"event">> := Event, <<"user">> := User, <<"text">> := _, <<"room">> := Room}) ->
    {decode_event(Event), User, Room}.

-spec encode_event(event() | error | message) ->
    binary().

encode_event(message) ->
    <<"message">>;
encode_event(create) ->
    <<"create">>;
encode_event(delete) ->
    <<"delete">>;
encode_event(join) ->
    <<"join">>;
encode_event(error) ->
    <<"error">>;
encode_event(leave) ->
    <<"leave">>.

-spec decode_event(binary()) ->
    event() | error | message.

decode_event(<<"create">>) ->
    create;
decode_event(<<"message">>) ->
    message;
decode_event(<<"join">>) ->
    join;
decode_event(<<"error">>) ->
    error;
decode_event(<<"leave">>) ->
    leave;
decode_event(<<"delete">>) ->
    delete.

-spec encode_reason(error_reason()) ->
    binary().

encode_reason(already_exists) ->
    <<"already_exists">>;
encode_reason(not_exists) ->
    <<"not_exists">>;
encode_reason(already_joined) ->
    <<"already_joined">>;
encode_reason(not_joined) ->
    <<"not_joined">>.

-spec decode_reason(binary()) ->
    error_reason().

decode_reason(<<"already_exists">>) ->
    already_exists;
decode_reason(<<"not_exists">>) ->
    not_exists;
decode_reason(<<"already_joined">>) ->
    already_joined;
decode_reason(<<"not_joined">>) ->
    not_joined.
