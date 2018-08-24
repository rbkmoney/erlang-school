-module(room_sup).

-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() ->
    {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    lager:notice("Rooms supervisor initialized"),
    SupArgs = #{
        strategy => one_for_one,
        intensity => 10,
        period => 10
    },
    Server = #{  %Single room so far
        id => room,
        start => {chat_server, start_link, [room1]}
    },
    {ok, {SupArgs, [Server]}}.
