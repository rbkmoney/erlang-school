-module(room_sup).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(supervisor).

-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/0]).
-export([create_room/1]).
-export([delete_room/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPE EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export_type([error/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type supervisor_args() :: supervisor:sup_flags().
-type error() :: running | restarting | not_found | simple_one_for_one.
-type child_args() :: supervisor:child_spec().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() ->
    {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec create_room(RoomId :: binary()) ->
    ok | error.

create_room(RoomId) ->
    ok = lager:notice("Creating room ~p", [RoomId]),
    Child = #{
        id => RoomId,
        type => worker,
        start => {chat_room, start_link, [RoomId]}
    },
    case supervisor:start_child(?MODULE, Child) of
        {ok, _} ->
            ok;
        {ok, _, _} ->
            ok;
        {error, Error} ->
            lager:notice("Supervisor can't start child with ~p", [Error]),
            {error, Error}
    end.

-spec delete_room(RoomId :: binary()) ->
    ok | {error, error()}.

delete_room(RoomId) ->
    ok = lager:info("Room supervisor is trying to delete child ~p", [RoomId]),
    case supervisor:terminate_child(?MODULE, RoomId) of
        ok ->
            supervisor:delete_child(?MODULE, RoomId);
        {error, Error} ->
            {error, Error}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init([]) ->
    {ok, {supervisor_args(), [child_args()]}}.

init([]) ->
    ok = lager:notice("Room supervisor initialized"),
    SupArgs = #{
        strategy => one_for_one,
        intensity => 10,
        period => 10
    },
    Server = #{  % Single room at the start
        id => <<"room1">>,
        type => worker,
        start => {chat_room, start_link, [<<"room1">>]}
    },
    {ok, {SupArgs, [Server]}}.
