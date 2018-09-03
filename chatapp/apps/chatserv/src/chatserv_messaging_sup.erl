-module(chatserv_messaging_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, chsv_messaging_sup).

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
    SupFlags = #{
        strategy => one_for_all
    },
    Children = [
        #{
            id => room_supervisor,
            start => {chatserv_room_sup, start_link, []},
            type => supervisor
        },
        #{
            id => room_manager,
            start => {chatserv_room_manager, start_link, []}
        }
    ],
    {ok, {SupFlags, Children}}.

%%%
%%% Internal functions
%%%
