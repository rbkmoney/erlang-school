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
    SupArgs = #{
        strategy => one_for_one,
        intensity => 0,
        period => 1
    },
    BotIds = [bot_1, bot_2, bot_3, bot_4],
    BotNames = ["FirstBot", "SecondBot", "ThirdBot", "FourthBot"],
    Bots = lists:map(
        fun({I, N}) ->
            #{id => I, start => {chat_bot, start_link, [N]}}
        end,
        lists:zip(BotIds, BotNames)
    ),
    {ok, {SupArgs, Bots}}.

%%====================================================================
%% Internal functions
%%====================================================================
