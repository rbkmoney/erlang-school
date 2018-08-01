-module(coolchat_sup).
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
    ChatRoom = #{
        id => chat_room,
        start => {chat_room, start_link, []}
    },
    BotsSup = #{
        id => bots_supervisor,
        start => {chatbot_sup, start_link, []}
    },
    {ok, {SupArgs, [ChatRoom, BotsSup]}}.

%%====================================================================
%% Internal functions
%%====================================================================
