-module(protocol).

% Protocol encodes values as tuples
% encodes tuples as JSONs and DECODS JSONs to messages for server

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([encode/2]).
-export([encode/3]).
-export([get_room_id/1]).
-export([message_to_json/1]).
-export([mesasge_to_client_json/3]).
-export([json_to_server_message/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type username() :: chat_server:username().
-type message() :: chat_server:message().
-type data() :: {message, username(), message()} | {atom(), username()}.
-type json() :: binary().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec encode(Atom :: atom(), Username :: username()) ->
        chat_server:broadcast_message().

encode(Atom, Username) ->
    {Atom, Username}.

-spec encode(message, Username :: username(), Message :: message()) ->
        chat_server:broadcast_message().

encode(message, Username, Message) ->
    {message, Username, Message}.

mesasge_to_client_json(Event, Body, Room) ->
    DataMap = create_client_map(Event, Body, Room),
    jiffy:encode(DataMap).

-spec message_to_json(Data :: data()) ->
    json().

message_to_json(Data) ->
    DataMap = create_json_map(Data),
    jiffy:encode(DataMap).

-spec json_to_server_message(binary(), pid()) ->
    {atom(), message(), atom(), pid()}.

json_to_server_message(Json, PID) ->
    DataMap = jiffy:decode(Json, [return_maps]),
    {Event, Message, RoomId} = decode_client_map(DataMap),
    {Event, Message, RoomId, PID}.

-spec get_room_id({term(), term(), atom(), term()}) ->
    atom().

get_room_id({_, _, Id, _}) ->
    Id.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec create_json_map(data()) ->
    #{event => atom(), user => username(), message => message()}
    | #{event => atom(), user => username()}.

create_client_map(Event, Body, Room) ->
    #{event => Event, body => Body, room => Room}.

create_json_map({message, Username, Message}) ->
    #{event => send_message, user => Username, message => Message};
create_json_map({Event, Username}) ->
    #{event => Event, user => Username}.

-spec decode_client_map(map()) ->
    {atom, binary(), atom()}.

decode_client_map(DataMap) ->
    Event = maps:get(<<"event">>, DataMap),
    Message = maps:get(<<"body">>, DataMap),
    RoomId = maps:get(<<"room">>, DataMap),
    {binary_to_atom(Event), Message, binary_to_atom(RoomId)}.

-spec binary_to_atom(binary()) ->
    atom().

binary_to_atom(Binary) ->
    list_to_atom(binary_to_list(Binary)).
