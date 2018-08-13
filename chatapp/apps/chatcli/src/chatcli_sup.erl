-module(chatcli_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%
%% API functions
%%

-spec start_link() ->
    {ok, Pid :: pid()} | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%
%% Supervisor callbacks
%%

-spec init([]) ->
    {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{},
    Children = [#{
        id => client,
        start => {chatcli_client, start_link, []}
    }],
    {ok, {SupFlags, Children}}.

%%
%% Internal functions
%%
