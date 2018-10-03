-module(server_SUITE).

-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile([export_all]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type groupName() :: atom().
-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MACROSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% General

-define(USER,      <<"Igor">>).
-define(MESSAGE,  <<"Hello">>).
-define(ROOM,     <<"room1">>).
-define(HOST,     "localhost").
-define(PORT,            8080).

% Commands

-define(CREATE_ROOM,            {create, <<"Igor">>, <<>>, ?ROOM}).
-define(SEND_MESSAGE, {send_message, <<"Igor">>, ?MESSAGE, ?ROOM}).
-define(DELETE_ROOM,            {delete, <<"Igor">>, <<>>, ?ROOM}).
-define(JOIN_ROOM,                {join, <<"Igor">>, <<>>, ?ROOM}).

% Errors

-define(ALREADY_IN_ROOM, {error, <<>>, <<"ALREADY IN THE ROOM">>, <<>>}).
-define(NOT_IN_ROOM,  {error, <<>>, <<"NOT JOINED TO THE ROOM">>, <<>>}).
-define(ALREADY_EXISTS,  {error, <<>>, <<"ROOM ALREADY EXISTS">>, <<>>}).
-define(NO_ROOM,                     {error, <<>>, <<"NO ROOM">>, <<>>}).

%%%%%%%%%%%%%%%%%%%%%%%%% TEST INITIALIZATION %%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() ->
    [{group, groupName()}].

all() ->
     [mega_test].

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

-spec mega_test(C :: config()) ->
    term().

mega_test(_C) ->
    % Have to test everything in one place to be able to use the same pid
    {ok, PID} = chat_client_client:start_link(?HOST, ?PORT),
    ok = chat_client_client:set_username(PID, ?USER),
    no_room = chat_client_client:join(PID, ?ROOM),
    ok = chat_client_client:create(PID, ?ROOM),
    already_exists = chat_client_client:create(PID, ?ROOM),
    already_joined = chat_client_client:join(PID, ?ROOM),
    ok = chat_client_client:send(PID, ?MESSAGE, ?ROOM),
    ok = chat_client_client:delete(PID, ?ROOM),
    not_joined = chat_client_client:send(PID, ?MESSAGE, ?ROOM).
