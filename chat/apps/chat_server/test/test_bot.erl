-module(test_bot).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(gen_server).

-export([init       /1]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([handle_info/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export_type([bot_opts/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type state() :: #{
    pid    := pid(),
    rooms  := rooms(),
    chain  := markov_chain:markov_chain(_),
    actions_left := action_counter(),
    delay := {non_neg_integer(), non_neg_integer()}
}.

-type chat_client_nodes() :: #{
    join    := markov_node(),
    leave   := markov_node(),
    create  := markov_node(),
    delete  := markov_node(),
    message := markov_node()
}.

-type bot_opts() :: #{
    name := user(),
    actions_left := action_counter(),
    rooms := rooms(),
    nodes := chat_client_nodes(),
    con_opts := con_opts(),
    initial_action := bot_actions(),
    delay := {non_neg_integer(), non_neg_integer()}
}.

-type con_opts() :: {host(), connection_port()}.
-type bot_actions() :: join | leave | create | delete | message.
-type host() :: string().
-type connection_port() :: non_neg_integer().
-type user()  :: library_protocol:user().
-type rooms() :: [library_protocol:room()].
-type action_counter() :: non_neg_integer().
-type markov_node() :: markov_node:markov_node(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link(bot_opts()) ->
    {ok, pid()}.

start_link(BotOpts) ->
    gen_server:start_link(?MODULE, BotOpts, []).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(bot_opts()) ->
    {ok, state(), non_neg_integer()}.

init(#{
        name := Name,
        actions_left := Actions,
        rooms := Rooms,
        nodes := Nodes,
        con_opts := {Host, Port},
        delay := {Delay, Spread},
        initial_action := InitialAction
}) ->
    {ok, PID} = chat_client_client:start_link(Host, Port),
    ok = chat_client_client:set_username(PID, Name),
    State = #{
        actions_left => Actions,
        rooms => Rooms,
        pid => PID,
        chain => markov_chain:create(Nodes, InitialAction),
        delay => {Delay, Spread}
    },
    {ok, State, generate_timeout(State)}.

-spec handle_call(term(), term(), state()) ->
    {reply, ok, state()}.

handle_call(_, _, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) ->
    {noreply, state()}.

handle_cast(_, State) ->
    {noreply, State}.

-spec handle_info
    (timeout, #{actions_left := 0}) ->
        {stop, normal, term()};
    (timeout, state()) ->
        {noreply, state(), non_neg_integer()}.

handle_info(timeout, #{actions_left := 0}) ->
    {stop, normal, #{}};

handle_info(timeout, #{actions_left := ActionsLeft, chain := MarkovChain} = State) ->
    NewChain = markov_chain:next_step(MarkovChain),
    Action = decide(NewChain),
    _ = make_action(Action, State),
    {noreply, State#{actions_left => ActionsLeft - 1, chain => NewChain}, generate_timeout(State)}.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec decide(MarkovNode :: markov_node()) ->
    library_protocol:event().

decide(MarkovChain) ->
    case markov_chain:curr_step(MarkovChain) of
        message ->
            {message, create_noise()};
        Action ->
            Action
    end.

-spec make_action(library_protocol:event(), state()) ->
    ok | library_protocol:error().

make_action(create, #{pid := PID, rooms := Rooms}) ->
    Room = choose_random_room(Rooms),
    chat_client_client:create(PID, Room);

make_action(join, #{pid := PID, rooms := Rooms}) ->
    Room = choose_random_room(Rooms),
    chat_client_client:join(PID, Room);

make_action({message, Text}, #{pid := PID, rooms := Rooms}) ->
    Room = choose_random_room(Rooms),
    chat_client_client:send(PID, Text, Room);

make_action(leave, #{pid := PID, rooms := Rooms}) ->
    Room = choose_random_room(Rooms),
    chat_client_client:leave(PID, Room);

make_action(delete, #{pid := PID, rooms := Rooms}) ->
    Room = choose_random_room(Rooms),
    chat_client_client:delete(PID, Room).

-spec create_noise() ->
    binary().

create_noise() ->
    Length = 3 + rand:uniform(7),
    erlang:list_to_binary([97 + rand:uniform(25) || _ <- lists:seq(1, Length)]).

-spec choose_random_room([library_protocol:room()]) ->
    library_protocol:room().

choose_random_room(Rooms) ->
    Index = rand:uniform(length(Rooms)),
    lists:nth(Index, Rooms).

-spec generate_timeout(state()) ->
    non_neg_integer().

generate_timeout(#{delay := {Delay, Spread}}) ->
    Delay + rand:uniform(Spread).
