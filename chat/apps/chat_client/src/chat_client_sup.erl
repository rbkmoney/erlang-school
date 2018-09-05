%%%-------------------------------------------------------------------
%% @doc chat_client top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ok = lager:notice("Application supervisor Initialized"),
    SupArgs = #{
        strategy => one_for_one,
        intensity => 10,
        period => 10
    },
    Client = #{
        id => client1,
        type => worker,
        start => {client, start_link, [client1]}
    },
    {ok, {SupArgs, [Client]}}.

%%====================================================================
%% Internal functions
%%====================================================================
