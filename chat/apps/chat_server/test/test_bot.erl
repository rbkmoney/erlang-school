-module(test_bot).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(gen_server).

-export([init       /1]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([handle_info/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type state() :: #{
    pid    := pid(),
    rooms  := rooms(),
    action := library_protocol:event(),
    actions_left := action_counter()
}.

-type req_nodes() :: #{
    join    := markov_node(),
    leave   := markov_node(),
    create  := markov_node(),
    delete  := markov_node(),
    message := markov_node()
}.

-type user()  :: library_protocol:user().
-type rooms() :: [library_protocol:room()].
-type action_counter() :: non_neg_integer().
-type markov_node() :: markov_node:markov_node().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start_link
    (Name :: user(), ActionCapacity :: action_counter(), Rooms :: rooms(), Nodes :: req_nodes()) ->
        {ok, pid()}.

start_link(Name, ActionCapacity, Rooms, Nodes) ->
    gen_server:start_link(?MODULE, {Name, ActionCapacity, Rooms, Nodes}, []).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%

-spec init({Name :: user(), ActionsLeft :: action_counter(), Rooms :: rooms()}) ->
    {ok, state(), non_neg_integer()}.

init({Name, ActionsLeft, Rooms, Nodes}) ->
    {ok, PID} = chat_client_client:start("localhost", 8080),
    ok = chat_client_client:set_username(PID, Name),
    State = #{
        actions_left => ActionsLeft,
        rooms => Rooms,
        pid => PID,
        nodes => Nodes,
        action => create
    },
    {ok, State, generate_timeout()}.

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
        {stop, normal};
    (timeout, state()) ->
        {noreply, state(), non_neg_integer()}.

handle_info(timeout, #{actions_left := 0}) ->
    ok = lager:debug("Bot with PID ~p expired his action limit, terminating"),
    {stop, normal, #{}};

handle_info(timeout, #{actions_left := ActionsLeft, action := Action} = State) ->
    ok = lager:debug("PID ~p Current action is ~p, actions left: ~p", [self(), Action, ActionsLeft]),
    make_action(Action),
    Node = get_node(Action, State),
    NextAction = next_action(Node),
    {noreply, State#{actions_left => ActionsLeft - 1, action => NextAction}, generate_timeout()}.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_node(library_protocol:event(), state()) ->
    markov_node().

get_node(Node, State) ->
    Nodes = maps:get(nodes, State),
    maps:get(Node, Nodes).

-spec next_action(MarkovNode :: markov_node()) ->
    library_protocol:event().

next_action(MarkovNode) ->
    decide(MarkovNode).

-spec decide(MarkovNode :: markov_node()) ->
    library_protocol:event().

decide(MarkovNode) -> % Picks an action with the greatest key, that is less then Value
    case markov_node:get_random(MarkovNode) of
        message ->
            {message, create_noise()};
        Action ->
            Action
    end.

-spec make_action(state()) ->
    ok | library_protocol:error().

make_action(#{pid := PID, rooms := Rooms, action := create}) ->
    Room = choose_random_room(Rooms),
    chat_client_client:create(PID, Room);

make_action(#{pid := PID, rooms := Rooms, action := join}) ->
    Room = choose_random_room(Rooms),
    chat_client_client:join(PID, Room);

make_action(#{pid := PID, rooms := Rooms, action := {message, Text}}) ->
    Room = choose_random_room(Rooms),
    chat_client_client:send(PID, Text, Room);

make_action(#{pid := PID, rooms := Rooms, action := leave}) ->
    Room = choose_random_room(Rooms),
    chat_client_client:leave(PID, Room);

make_action(#{pid := PID, rooms := Rooms, action := delete}) ->
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

-spec generate_timeout() ->
    non_neg_integer().

generate_timeout() ->
    20 + rand:uniform(20). % 20-40ms delay between actions
