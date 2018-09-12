-module(server_SUITE).

-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile([export_all]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type groupName() :: atom().
-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MACROSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(HOST, "localhost").
-define(PORT, 8080).
-define(ID, <<"client1">>).
-define(USER, <<"Igor">>).
-define(MESSAGE, <<"Hello">>).
-define(ROOM, <<"room1">>).
-define(NONEXISTENT_ROOM_MESSAGE, {register, <<"Igor">>, <<"">>, <<"noroom">>}).
-define(NO_ROOM_REPLY, {error, <<"">>, <<"NO ROOM">>, <<"">>}).
-define(EXPECTED_MESSAGES,
    [
        {send_message, <<"Incognito">>, <<"Hello">>, <<"room1">>},
        {joined, <<"Incognito">>, <<>>, <<"room1">>},
        {success, <<>>, <<>>, <<"room1">>}
    ]).

%%%%%%%%%%%%%%%%%%%%%%%%% TEST INITIALIZATION %%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() ->
    [{group, groupName()}].

all() ->
     [
        {group, impossible_interactions},
        {group, basic_interactions}
     ].

-spec groups() ->
    [{groupName(), [sequence], [atom()]}].

groups() ->
    [
        {basic_interactions, [sequence], [
            join_room,
            send_message,
            receive_message
        ]},
        {impossible_interactions, [sequence], [
            cant_send_to_nonexistent_room
        ]}
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%% SUITE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init_per_suite(C :: config()) ->
    config().

init_per_suite(C) ->
    {ok, Apps1} = application:ensure_all_started(chat_server),
    {ok, Apps2} = application:ensure_all_started(library),
    {ok, Apps3} = application:ensure_all_started(chat_client),
    [{apps, [Apps1, Apps2, Apps3]} | C].

-spec end_per_suite(C :: config()) ->
    term().

end_per_suite(C) ->
    [application:stop(App) || App <- ?config(apps, C)].

%%%%%%%%%%%%%%%%%%%%%%%%%% BASIC INTERACTIONS %%%%%%%%%%%%%%%%%%%%%%%%%

-spec join_room(C :: config()) ->
    term().

join_room(_C) ->
    connected = client:connect(?HOST, ?PORT, ?ID),
    ok = client:join(?ID, ?ROOM).

-spec send_message(C :: config()) ->
    term().

send_message(_C) ->
    client:send(?ID, ?MESSAGE, ?ROOM).

-spec receive_message(C :: config()) ->
    term().

receive_message(_C) ->
    timer:sleep(150), % Give server some time to handle and respond
    List = client:get_messages(?ID),
    ?EXPECTED_MESSAGES = List.

%%%%%%%%%%%%%%%%%%%%%%% IMPOSSIBLE INTERACTIONS %%%%%%%%%%%%%%%%%%%%%%%

-spec cant_send_to_nonexistent_room(C :: config()) ->
    term().

cant_send_to_nonexistent_room(_C) ->
    chat_room:send(?NONEXISTENT_ROOM_MESSAGE, self()),
    ?NO_ROOM_REPLY = receive_reply().

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec receive_reply() ->
    protocol:source_message() | nothing.

receive_reply() ->
    receive
        {send, Reply} ->
            Reply
    after 2500 ->
        nothing
    end.
