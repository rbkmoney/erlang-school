-module(test_bot).

%%%%%%%%%%%%%%%%%%%%%%%%%% BEHAVIOUR EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%

-behaviour(gen_server).

-export([init       /1]).
-export([handle_cast/2]).
-export([handle_call/3]).
-export([handle_info/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_link/3]).

start_link(BotName, ActionCapacity, Rooms) ->
    gen_server:start_link(?MODULE,{BotName, ActionCapacity, Rooms},[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type state() :: #{
    actions_left := non_neg_integer(),
    rooms := [library_protocol:room()],
    pid := pid(),
    action := library_protocol:event()
}.

-type markov_node() :: #{
    0       := library_protocol:event(),
    float() := library_protocol:event(),
    float() := library_protocol:event(),
    float() := library_protocol:event(),
    float() := library_protocol:event()
}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MACROSES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(JOIN,    #{0 => create, 0.2 => join, 0.3 => message, 0.8 => leave, 0.9 => delete}).
-define(LEAVE,   #{0 => create, 0.2 => join, 0.5 => message, 0.7 => leave, 0.8 => delete}).
-define(CREATE,  #{0 => create, 0.1 => join, 0.2 => message, 0.8 => leave, 0.9 => delete}).
-define(DELETE,  #{0 => create, 0.3 => join, 0.6 => message, 0.8 => leave, 0.9 => delete}).
-define(MESSAGE, #{0 => create, 0.1 => join, 0.2 => message, 0.7 => leave, 0.9 => delete}).

%%%%%%%%%%%%%%%%%%%%%%%%%% CALLBACK FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%

-spec init({binary(), ActionsLeft :: non_neg_integer(), Rooms :: [library_protocol:room()]}) ->
    {ok, state(), non_neg_integer()}.

init({BotName, ActionsLeft, Rooms}) ->
    {ok, PID} = chat_client_client:start("localhost", 8080),
    ok = chat_client_client:set_username(PID, BotName),
    State = #{
        actions_left => ActionsLeft,
        rooms => Rooms,
        pid => PID,
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
    (timeout, #{actions_left :=0}) ->
        {stop, no_actions_remaining};
    (timeout, state()) ->
        {noreply, state(), non_neg_integer()}.

handle_info(timeout, #{actions_left := 0}) ->
    ok = lager:debug("Bot with PID ~p expired his action limit, terminating"),
    {stop, no_actions_remaining};

handle_info(timeout, #{actions_left := ActionsLeft, action := Action} = State) ->
    ok = lager:debug("PID ~p Current action is ~p, actions left: ~p", [self(), Action, ActionsLeft]),
    make_action(Action),
    Node = get_node(Action),
    NextAction = next_action(Node),
    {noreply, State#{actions_left => ActionsLeft - 1, action => NextAction}, generate_timeout()}.

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_node(library_protocol:event()) ->
    markov_node().

get_node(create) ->
    ?CREATE;

get_node(join) ->
    ?JOIN;

get_node({message, _}) ->
    ?MESSAGE;

get_node(leave) ->
    ?LEAVE;

get_node(delete) ->
    ?DELETE.

-spec next_action(MarkovNode :: markov_node()) ->
    library_protocol:event().

next_action(MarkovNode) ->
    decide(rand:uniform(), MarkovNode).

-spec decide(Value :: float(), MarkovNode :: markov_node()) ->
    library_protocol:event().

decide(Value, MarkovNode) -> % Picks an action with the greatest key, that is less then Value
    Keys = maps:keys(MarkovNode),
    case maps:get(lists:max([Item =< Value || Item <- Keys]), MarkovNode) of
        message ->
            {message, create_noice()};
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

-spec create_noice() ->
    binary().

create_noice() ->
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
