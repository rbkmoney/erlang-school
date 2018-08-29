-module(room_sup).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(supervisor).

-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/0]).
-export([create_room/1]).
-export([delete_room/1]).
-export([get_children/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type supervisor_args() :: supervisor:sup_flags().
-type child_args() :: supervisor:child_spec().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() ->
    {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

get_children() ->
    supervisor:which_children(?MODULE).

-spec create_room(RoomId :: atom()) ->
    supervisor:startchild_ret() | {error, already_exists}.

create_room(RoomId) ->
    lager:notice("Creating room ~p", [RoomId]),
    Child = #{
        id => RoomId,
        type => worker,
        start => {chat_room, start_link, [RoomId]}
    },
    supervisor:start_child(room_sup, Child).

delete_room(RoomId) ->
    lager:info("Room supervisor is trying to delete child ~p", [RoomId]),
    supervisor:terminate_child(room_sup, RoomId),
    supervisor:delete_child(room_sup, RoomId).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init([]) ->
    {ok, {supervisor_args(), [child_args()]}}.

init([]) ->
    lager:notice("Room supervisor initialized"),
    SupArgs = #{
        strategy => one_for_one,
        intensity => 10,
        period => 10
    },
    Server = #{  %Single room at the start
        id => room1,
        start => {chat_room, start_link, [room1]}
    },
    {ok, {SupArgs, [Server]}}.
