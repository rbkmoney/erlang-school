-module(chatserv_socket_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, chsv_socket_sup).

%%%
%%% API functions
%%%
-spec start_link() ->
    chatserv_sup:sv_sl_result().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%
%%% Supervisor callbacks
%%%
-spec init(Args :: term()) ->
    chatserv_sup:sv_init_result().
init([]) ->
    lager:notice("~p supervisor starting", [?SERVER]),
    SupFlags = #{
        stategy => simple_one_for_one
    },
    Children = [#{
        id => sockn,
        start => {chatserv_socket, start_link, []},
        shutdown => brutal_kill
    }],
    {ok, {SupFlags, Children}}.

%%%
%%% Internal functions
%%%
