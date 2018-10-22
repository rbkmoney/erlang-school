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
-type bot_opts() ::test_bot:bot_opts().
-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MACROSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% General

-define(USER,      <<"Igor">>).
-define(MESSAGE,  <<"Hello">>).
-define(ROOM,     <<"room1">>).
-define(HOST,     "localhost").
-define(PORT,            8080).
-define(TIMEOUT,           20).
-define(ACTIONS_NUMBER,    20).
-define(CRITICAL_TIMEOUT, ?TIMEOUT * ?ACTIONS_NUMBER * 2).

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
    Vars = setup_variables(),
    PIDs = monitor(Vars),
    ct:log("Created processes ~p", [PIDs]),
    collect(PIDs).

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec setup_variables() ->
    [bot_opts()].

setup_variables() ->
    Rooms  = [<<"room1">>, <<"room2">>],
    Names  = [<<"Adam">>, <<"Betty">>, <<"Charlie">>, <<"Donald">>, <<"Edna">>],
    ConOpts = {?HOST, ?PORT},
    [
        #{
            rooms => Rooms,
            name => Item,
            timeout => ?TIMEOUT,
            nodes => ?NODE_MAP,
            actions_left => ?ACTIONS_NUMBER,
            initial_action => create,
            con_opts => ConOpts
        } || Item <- Names
    ].

-spec monitor(bot_opts()) ->
    [pid()].

monitor(Vars) ->
    OkPIds = [test_bot:start_link(Item) || Item <- Vars],
    PIDs = [Item || {ok, Item} <- OkPIds],
    [erlang:monitor(process, Item) || Item <- PIDs],
    PIDs.

-spec collect([pid()]) ->
        ok.

collect([]) -> ok;

collect([PID | Tail]) ->
    receive
        {'DOWN', _, process, PID, normal} ->
            ct:log("Collected process ~p", [PID]),
            collect(Tail)
    after ?CRITICAL_TIMEOUT ->
        error(timeout)
    end.
