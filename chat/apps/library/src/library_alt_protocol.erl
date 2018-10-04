-module(library_alt_protocol).

-export([encode/1]).
-export([decode/1]).

-type text() :: binary().
-type room() :: binary().
-type user() :: binary().
-type event() :: join | leave | create | delete | {message, text()}.
-type succ_message() :: {event(), user(), room()}.
-type error_reason() :: already_exists | not_exists | already_joined | not_joined.
-type error() :: {error, error_reason()}.
-type decoded() :: succ_message() | error_reason().
-type mapped_json() :: #{event := binary(), text := text(), user := user(), room := room()}.

-spec encode(Message :: decoded()) ->
    jiffy:json_value().

encode(Message) ->
    Map = to_map(Message),
    jiffy:encode(Map).

-spec decode(Json :: jiffy:json_value()) ->
    decoded().

decode(Json) ->
    Map = jiffy:decode(Json, [return_maps]),
    Map1 = normalize_map(Map),
    from_map(Map1).

-spec to_map(decoded()) ->
    mapped_json().

to_map({{message, Text}, User, Room}) ->
    #{event => encode_event(message), text => Text, user => User, room => Room};

to_map({error, Reason}) ->
    #{event => encode_event(error), text => encode_reason(Reason), user => <<>>, room => <<>>};

to_map({Event, User, Room}) ->
    #{event => encode_event(Event), text => <<>>, user => User, room => Room}.

normalize_map(#{<<"event">> := Event} = Map) ->
    Map#{<<"event">> => decode_event(Event)}.

from_map(#{<<"event">> := error, <<"text">> := Reason}) ->
    {error, decode_reason(Reason)};

from_map(#{<<"event">> := message, <<"user">> := User, <<"text">> := Text, <<"room">> := Room}) ->
    {{message, Text}, User, Room};

from_map(#{<<"event">> := Event, <<"user">> := User, <<"text">> := Text, <<"room">> := Room}) ->
    {Event, User, Room}.

-spec encode_event(event()) ->
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
    event().

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

encode_reason(already_exists) ->
    <<"already_exists">>;
encode_reason(not_exists) ->
    <<"not_exists">>;
encode_reason(already_joined) ->
    <<"already_joined">>;
encode_reason(not_joined) ->
    <<"not_joined">>.

decode_reason(<<"already_exists">>) ->
    already_exists;
decode_reason(<<"not_exists">>) ->
    not_exists;
decode_reason(<<"already_joined">>) ->
    already_joined;
decode_reason(<<"not_joined">>) ->
    not_joined.
