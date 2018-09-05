%%%-------------------------------------------------------------------
%% @doc chat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_sup).

-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

-type supervisor_args() :: supervisor:sup_flags().
-type child_args() :: supervisor:child_spec().

%% API.

-spec start_link() ->
    {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.
-spec init([]) ->
    {ok, {supervisor_args(), [child_args()]}}.

init([]) ->
    ok = lager:notice("Application supervisor Initialized"),
    SupArgs = #{
        strategy => one_for_one,
        intensity => 10,
        period => 10
    },
    RoomManager = #{
        id => manager,
        type => worker,
        start => {room_manager, start_link, []}
    },
    RoomSupervisor = #{
        id => room_supervisor,
        type => supervisor,
        start => {room_sup, start_link, []}
    },
    {ok, {SupArgs, [RoomManager, RoomSupervisor]}}.
