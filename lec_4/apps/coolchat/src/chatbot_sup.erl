-module(chatbot_sup).
-author("Kehitt").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() ->
    {ok, pid()} | {error, _}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupArgs = #{},
    BotNames = ["FirstBot", "SecondBot", "ThirdBot", "FourthBot"],
    Bots = lists:map(
        fun(N) ->
            #{
                id => list_to_atom("id" ++ N),
                start => {chat_bot, start_link, [N]}
            }
        end,
        BotNames
    ),
    {ok, {SupArgs, Bots}}.

%%====================================================================
%% Internal functions
%%====================================================================
