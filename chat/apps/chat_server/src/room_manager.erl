-module(room_manager).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(gen_server).

-export([init/1]).
-export([handle_cast/2]).
-export([handle_call/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([rooms/0]).
-export([room_pid/1]).
-export([start_link/0]).
-export([create_room/1]).
-export([delete_room/1]).
-export([room_exists/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type match() :: [{n, l, {chat_room, binary()}}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link() ->
    {ok, pid()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, undefined, []).

-spec room_exists(Id :: binary()) ->
    boolean().

room_exists(Id) ->
    gen_server:call(?MODULE, {room_exists, Id}).

-spec create_room(Id :: binary()) ->
    ok | already_exists.

create_room(Id) ->
    gen_server:call(?MODULE, {create_room, Id}).

-spec delete_room(Id :: binary()) ->
    ok | not_found.

delete_room(Id) ->
    gen_server:call(?MODULE, {delete_room, Id}).

-spec room_pid(Id :: binary()) ->
    not_found | pid().

room_pid(Id) ->
    gen_server:call(?MODULE, {room_pid, Id}).

-spec rooms() ->
    [binary()].

rooms() ->
    gen_server:call(?MODULE, rooms).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(undefined) ->
    {ok, stateless}.

init(undefined) ->
    ok = lager:notice("Room manager initialized"),
    {ok, stateless}.

-spec handle_cast(term(), State :: stateless) ->
    {noreply, stateless}.

handle_cast(_, State) ->
    {noreply, State}.

-spec handle_call
    ({create_room, Id :: binary}, _From :: {pid(), _}, State :: stateless) ->
        {reply, ok | {error, room_initialization_timeout} | already_exists, stateless};

    ({room_exists, Id :: binary}, _From :: {pid(), _}, State :: stateless) ->
        {reply, boolean(), stateless};

    ({room_pid, Id :: binary}, _From :: {pid(), _}, State :: stateless) ->
        {reply, pid() | not_found, stateless};

    ({delete_room, Id :: binary}, _From :: {pid(), _}, State :: stateless) ->
        {reply, ok | not_found, stateless};

    (rooms, _From :: {pid(), _}, State :: stateless) ->
        {reply, [binary()], stateless}.

handle_call({create_room, Id}, _From, State) ->
    Reply = case room_exists_(Id) of
        false ->
            {ok, PID} = chat_room:start_link({Id, self()}),
            receive
                {register, Id, PID} ->
                    true = gproc:reg_other({n, l, {chat_room, Id}}, PID),
                    ok = lager:info("Registered room Id: ~p, pid: ~p", [Id, PID]),
                    ok
            after 250 ->
                    {error, room_initialization_timeout}
            end;
        true ->
            already_exists
    end,
    {reply, Reply, State};

handle_call({room_exists, Id}, _From, State) ->
    Reply = room_exists_(Id),
    {reply, Reply, State};

handle_call({room_pid, Id}, _From, State) ->
    Reply = case gproc:lookup_local_name({chat_room, Id}) of
        undefined ->
            not_found;
        PID ->
            PID
    end,
    {reply, Reply, State};

handle_call({delete_room, Id}, _From, State) ->
    Reply = case room_exists_(Id) of
        false ->
            not_found;
        true ->
            PID = gproc:lookup_local_name({chat_room, Id}),
            ok = lager:info("Room manager deletes room ~p", [Id]),
            true = gproc:unreg_other({n, l, {chat_room, Id}}, PID),
            shutdown_ok = chat_room:stop(PID),
            ok
    end,
    {reply, Reply, State};

handle_call(rooms, _From, State) ->
    RawRes = raw_room_list(),
    Reply = [extract_room(Room) || Room <- RawRes],
    {reply, Reply, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec room_exists_(Id :: binary()) ->
    boolean().

room_exists_(Id) ->
    case gproc:lookup_local_name({chat_room, Id}) of
        undefined ->
            false;
        _ ->
            true
    end.

-spec raw_room_list() ->
    [match()].

raw_room_list() ->
    MatchHead = '_',
    Guard = [],
    Result = ['$$'],
    gproc:select([{MatchHead, Guard, Result}]). % same as select *

-spec extract_room(match()) ->
    binary().

extract_room([{_, _, {chat_room, Id}}, _, _]) ->
    Id.
