-module(protocol).

% Protocol encodes values as tuples
% encodes tuples as JSONs and DECODS JSONs to messages for server

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([encode/2]).
-export([encode/3]).
-export([message_to_json/1]).
-export([json_to_server_message/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec encode(atom(), chat_server:message()) ->
        {atom(), chat_server:message()}.

encode(Atom, Username) ->
    {Atom, Username}.

-spec encode(message, Username :: chat_server:username(), Message :: chat_server:message()) ->
    {message, chat_server:username(), chat_server:message()}.

encode(message, Username, Message) ->
    {message, Username, Message}.

message_to_json(Data) ->
    DataMap = create_json_map(Data),
    jiffy:encode(DataMap).

json_to_server_message(Json, PID) ->
    DataMap = jiffy:decode(Json, [return_maps]),
    {Event, Message} = decode_client_map(DataMap),
    {Event, Message, PID}.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

create_json_map({message, Username, Message}) ->
    #{event => send_message, user => Username, message => Message};
create_json_map({Event, Username}) ->
    #{event => Event, user => Username}.

decode_client_map(DataMap) ->
    Event = maps:get(<<"event">>, DataMap),
    Message = maps:get(<<"body">>, DataMap),
    {binary_to_atom(Event), Message}.

binary_to_atom(Binary) ->
    list_to_atom(binary_to_list(Binary)).
