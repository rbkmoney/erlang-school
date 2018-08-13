-module(chatserv_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, chsv_client_sup).

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
    lager:notice("~p root supervisor starting", [?SERVER]),
    SupFlags = #{
        strategy => one_for_all
    },
    Children = [
        #{
            id => socket_manager,
            start => {chatserv_socket_manager, start_link, []}
        },
        #{
            id => socket_supervisor,
            start => {chatserv_socket_sup, start_link, []},
            type => supervisor
        }
    ],
    {ok, {SupFlags, Children}}.

%%%
%%% Internal functions
%%%
