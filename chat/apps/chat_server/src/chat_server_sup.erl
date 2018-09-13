-module(chat_server_sup).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(supervisor).

-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type supervisor_args() :: supervisor:sup_flags().
-type child_args() :: supervisor:child_spec().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() ->
    {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init([]) ->
    {ok, {supervisor_args(), [child_args()]}}.

init([]) ->
    ok = lager:notice("Chat_server supervisor Initialized"),
    SupArgs = #{
        strategy => one_for_one,
        intensity => 10,
        period => 10
    },
    Room_manager = #{
        id => manager,
        type => worker,
        start => {room_manager, start_link, []}
    },
    {ok, {SupArgs, [Room_manager]}}.
