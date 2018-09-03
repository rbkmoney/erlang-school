-module(chatcli_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_client/3,
    stop_client/1
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

-spec start_client(inet:hostname(), inet:port_number(), fun()) ->
    {ok, pid()} | {error, _}.
start_client(HostName, Port, MessageCB) ->
    supervisor:start_child(?SERVER, [HostName, Port, MessageCB]).

-spec stop_client(pid()) ->
    ok | {error, term()}.
stop_client(Pid) ->
    supervisor:terminate_child(?SERVER, Pid).

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
