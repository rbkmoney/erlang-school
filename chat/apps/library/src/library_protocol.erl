-module(library_protocol).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([encode     /1]).
-export([decode     /1]).
-export([match_error/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MACROSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(NOT_SUBSCRIBED_ERROR,  {error, <<>>, <<"NOT JOINED TO THE ROOM">>, <<>>}).
-define(ALREADY_SUBSCRIBED_ERROR, {error, <<>>, <<"ALREADY IN THE ROOM">>, <<>>}).
-define(ALREADY_EXISTS_ERROR,     {error, <<>>, <<"ROOM ALREADY EXISTS">>, <<>>}).
-define(NO_ROOM_ERROR,                        {error, <<>>, <<"NO ROOM">>, <<>>}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPE EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export_type([room          /0]).
-export_type([event         /0]).
-export_type([error         /0]).
-export_type([active_event  /0]).
-export_type([source_message/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type error() :: already_exists | already_joined | no_room | not_joined.
-type source_message() :: {event(), binary(), binary(), room()}.
-type event() :: active_event() | error | send_message.
-type active_event() :: join | leave | delete | create.
-type binary_key_map() :: #{binary() => binary() | atom()}.
-type room() :: binary().

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

-spec match_error(Message :: source_message()) ->
    error().

match_error({error, _, _, _} = Message) ->
    case Message of
        ?NO_ROOM_ERROR ->
            no_room;
        ?ALREADY_EXISTS_ERROR ->
            already_exists;
        ?ALREADY_SUBSCRIBED_ERROR ->
            already_joined;
        ?NOT_SUBSCRIBED_ERROR ->
            not_joined
    end.

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
