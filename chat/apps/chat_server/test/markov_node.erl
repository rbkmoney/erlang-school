-module(markov_node).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([create    /1]).
-export([get_random/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPE EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export_type([markov_node/0]).
-export_type([event_possibility_map/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type event_possibility_map() :: #{
    term() := float(),
    term() => float()
}.

-type markov_node() :: #{
    0       := term(),
    float() => term()
}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec create(EventMap :: event_possibility_map()) ->
    markov_node() | {error, sum_is_not_1}.

create(EventMap) ->
    Possibilities = maps:values(EventMap),
    case lists:sum(Possibilities) of
        1.0 ->
            create_node(EventMap);
        _ ->
            {error, sum_is_not_1}
    end.

-spec get_random(MarkovNode :: markov_node()) ->
    term().

get_random(MarkovNode) ->
    Value = rand:uniform(),
    Keys = maps:keys(MarkovNode),
    maps:get(lists:max([Item || Item <- Keys, Item =< Value]), MarkovNode).

%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%

create_node(EventMap) ->
    create_node(maps:keys(EventMap), EventMap, 0, #{}).

create_node([], _, _, Result) ->
    Result;

create_node([Event | Rest] = _Events, EventMap, Acc, Result) ->
    Possibility = maps:get(Event, EventMap),
    create_node(Rest, EventMap, round(Acc + Possibility, 5), Result#{Acc => Event}).

round(Number, Precision) ->
    P = math:pow(10, Precision),
    round(Number * P) / P.
