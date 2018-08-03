%%%-------------------------------------------------------------------
%% @doc simpleChat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(simple_chat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

%There must be a spec for supervisor:start_link somewhere
-spec start_link() ->
    {ok,pid()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}

%-spec init([]) ->
%    {ok,map(),list()}.
init([]) ->
    lager:info("Application supervisor initialized"),
    SupArgs = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    Room = #{
        id => my_room,
        start => {rooms,start_link,[]}
    },
    Bots = #{
        id => bots,
        start => {chatbots_sup,start_link,[]}
    },
    {ok,{SupArgs,[Room,Bots]}}.

%%====================================================================
%% Internal functions
%%====================================================================
