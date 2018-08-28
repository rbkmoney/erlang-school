-module(chatcli_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_client/2
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%
%% API functions
%%
-spec start_link() ->
    chatserv_sup:sv_sl_result().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_client(inet:hostname(), inet:port_number()) ->
    {ok, pid()} | {error, _}.
start_client(HostName, Port) ->
    supervisor:start_child(?SERVER, [HostName, Port]).

%%
%% Supervisor callbacks
%%
-spec init(Args :: term()) ->
    chatserv_sup:sv_init_result().
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one
    },
    Children = [#{
        id => roomn,
        start => {chatcli_client, start_link, []},
        restart => permanent,
        shutdown => brutal_kill
    }],
    {ok, {SupFlags, Children}}.

%%
%% Internal functions
%%
