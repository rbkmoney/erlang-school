-module(server_SUITE).

-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-compile([export_all]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type group_name() :: atom().
-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MACROSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% General

-define(USER,      <<"Igor">>).
-define(MESSAGE,  <<"Hello">>).
-define(ROOM,     <<"room1">>).
-define(HOST,     "localhost").
-define(PORT,            8080).

% Commands

-define(JOIN_ROOM,                      {join, ?USER, ?ROOM}).
-define(CREATE_ROOM,                  {create, ?USER, ?ROOM}).
-define(DELETE_ROOM,                  {delete, ?USER, ?ROOM}).
-define(SEND_MESSAGE,    {{message, ?MESSAGE}, ?USER, ?ROOM}).

% Errors

-define(NO_ROOM,                {error, no_room}).
-define(NOT_IN_ROOM,         {error, not_joined}).
-define(ALREADY_EXISTS,  {error, already_exists}).
-define(ALREADY_IN_ROOM, {error, already_joined}).


%%%%%%%%%%%%%%%%%%%%%%%%% TEST INITIALIZATION %%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() ->
    [{group, group_name()}].

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
    {error, not_exists} = chat_client_client:join(PID, ?ROOM),
    ok = chat_client_client:create(PID, ?ROOM),
    {error, already_exists} = chat_client_client:create(PID, ?ROOM),
    {error, already_joined} = chat_client_client:join(PID, ?ROOM),
    ok = chat_client_client:send(PID, ?MESSAGE, ?ROOM),
    ok = chat_client_client:delete(PID, ?ROOM),
    {error, not_joined} = chat_client_client:send(PID, ?MESSAGE, ?ROOM).
