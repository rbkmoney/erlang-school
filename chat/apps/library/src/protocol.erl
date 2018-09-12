-module(protocol).

% На вход подаем {atom, binary, binary, binary}
% На выходе получаем binary
% На вход подаем binary
% На выходе получаем {atom, binary, binary, binary}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([encode/1]).
-export([decode/1]).

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

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec raw_to_map(source_message()) ->
    binary_key_map().

raw_to_map({Event, User, Message, Room}) ->
    #{<<"event">> => encode_event(Event), <<"user">> => User, <<"message">> => Message, <<"room">> => Room}.

-spec map_to_raw(binary_key_map()) ->
    source_message().

map_to_raw(#{<<"event">> := Event, <<"user">> := User, <<"message">> := Msg, <<"room">> := Room}) ->
    {decode_event(Event), User, Msg, Room}.

-spec encode_event(event()) ->
    binary().

encode_event(send_message) ->
    <<"send_message">>;
encode_event(register) ->
    <<"register">>;
encode_event(joined) ->
    <<"joined">>;
encode_event(error) ->
    <<"error">>;
encode_event(left) ->
    <<"left">>;
encode_event(success) ->
    <<"success">>.
% Может бросать ошибку если аргумент неверный?

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
    success.
% Может бросать ошибку если аргумент неверный?
