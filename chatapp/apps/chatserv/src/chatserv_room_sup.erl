-module(chatserv_room_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

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
    SupFlags = #{},
    Children = [],
    {ok, {SupFlags, Children}}.

%%%
%%% Internal functions
%%%
