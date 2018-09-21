-module(library_protocol).

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

-type event() :: create | join | error | send_message | leave | delete.
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
% Может бросать ошибку если аргумент неверный?

-spec decode_event(binary()) ->
    event().

decode_event(<<"create">>) ->
    create;
decode_event(<<"send_message">>) ->
    send_message;
decode_event(<<"join">>) ->
    join;
decode_event(<<"error">>) ->
    error;
decode_event(<<"leave">>) ->
    leave;
decode_event(<<"delete">>) ->
    delete.
% Может бросать ошибку если аргумент неверный?
