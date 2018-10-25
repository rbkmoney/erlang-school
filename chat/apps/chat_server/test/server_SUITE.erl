-module(server_SUITE).

-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% tests descriptions

-export([all           /0]).
-export([init_per_suite/1]).
-export([end_per_suite /1]).

%% tests

-export([mega_test                  /1]).
-export([randomized_multiclient_test/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type group_name() :: atom().
-type bot_opts() :: test_bot:bot_opts().
-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MACROSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% General

-define(USER,      <<"Igor">>).
-define(MESSAGE,  <<"Hello">>).
-define(ROOM,     <<"room1">>).
-define(HOST,     "localhost").
-define(PORT,            8080).
-define(DELAY,             20).
-define(SPREAD,            20).
-define(ACTIONS_NUMBER,    20).
-define(CRITICAL_TIMEOUT, (?DELAY + ?SPREAD) * ?ACTIONS_NUMBER * 2).

% Probability maps

-define(JOIN_NODE,    #{create => 0.2, join => 0.1, message => 0.5, leave => 0.1, delete => 0.1}).
-define(LEAVE_NODE,   #{create => 0.2, join => 0.3, message => 0.2, leave => 0.1, delete => 0.2}).
-define(CREATE_NODE,  #{create => 0.1, join => 0.1, message => 0.6, leave => 0.1, delete => 0.1}).
-define(DELETE_NODE,  #{create => 0.3, join => 0.3, message => 0.2, leave => 0.1, delete => 0.1}).
-define(MESSAGE_NODE, #{create => 0.1, join => 0.1, message => 0.5, leave => 0.2, delete => 0.1}).

% Node map

-define(NODE_MAP, #{
    join    => markov_node:create(?JOIN_NODE),
    leave   => markov_node:create(?LEAVE_NODE),
    create  => markov_node:create(?CREATE_NODE),
    delete  => markov_node:create(?DELETE_NODE),
    message => markov_node:create(?MESSAGE_NODE)
}).

%%%%%%%%%%%%%%%%%%%%%%%%% TEST INITIALIZATION %%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() ->
    [{group, group_name()}].

all() ->
    [mega_test, randomized_multiclient_test].

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

-spec randomized_multiclient_test(C :: config()) ->
    term().

randomized_multiclient_test(_C) ->
    BotOptsList = setup_variables(),
    PIDs = start_and_monitor_bots(BotOptsList),
    lists:foreach(fun collect_process/1, PIDs).

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec setup_variables() ->
    [bot_opts()].

setup_variables() ->
    ConOpts = {?HOST, ?PORT},
    Rooms  = [<<"room1">>, <<"room2">>],
    Names  = [<<"Adam">>, <<"Betty">>, <<"Charlie">>, <<"Donald">>, <<"Edna">>],
    [
        #{
            name  => Item,
            rooms => Rooms,
            delay => {?DELAY, ?SPREAD},
            nodes => ?NODE_MAP,
            con_opts => ConOpts,
            initial_action => create,
            actions_left => ?ACTIONS_NUMBER
        } || Item <- Names
    ].

-spec start_and_monitor_bots([bot_opts()]) ->
    [pid()].

start_and_monitor_bots(BotOptsList) ->
    OkPids = lists:map(fun test_bot:start_link/1, BotOptsList),
    PIDs = [Item || {ok, Item} <- OkPids],
    [erlang:monitor(process, Item) || Item <- PIDs],
    PIDs.

-spec collect_process(pid()) ->
    ok.

collect_process(PID) ->
    receive
        {'DOWN', _, process, PID, normal} ->
            ok
    after ?CRITICAL_TIMEOUT ->
        error(timeout)
    end.
