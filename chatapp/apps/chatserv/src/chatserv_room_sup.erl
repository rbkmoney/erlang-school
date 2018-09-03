-module(chatserv_room_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_room/2
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, chsv_room_sup).

%%
%% API functions
%%
-spec start_link() ->
    chatserv_sup:sv_sl_result().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_room(chatlib_proto:room_id_direct(), chatlib_proto:room_name()) ->
    {ok, pid()} | {error, _}.
start_room(RoomId, RoomName) ->
    supervisor:start_child(chsv_room_sup, [RoomId, RoomName]).

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
        start => {chatserv_room, start_link, []},
        restart => permanent,
        shutdown => brutal_kill
    }],
    {ok, {SupFlags, Children}}.

%%
%% Internal functions
%%
