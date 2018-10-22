-module(markov_chain).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([create   /2]).
-export([next_step/1]).
-export([curr_step/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPE EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export_type([markov_chain/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TYPES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type node_map(T) :: #{
    T := markov_node:markov_node(T),
    T => markov_node:markov_node(T)
}.

-opaque markov_chain(T) :: #{
    nodes := node_map(T),
    curr_step := T
}.

-type node_undef_error() :: {error, node_undefined}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec create(NodesMap :: node_map(T), InitialNode :: markov_node:markov_node(T)) ->
    markov_chain(T) | node_undef_error().

create(NodesMap, InitialNode) ->
    case maps:is_key(InitialNode, NodesMap) of
        true ->
            #{nodes => NodesMap, curr_step => InitialNode};
        false ->
            {error, node_undefined}
    end.

-spec next_step(markov_chain(T)) ->
    markov_chain(T) | node_undef_error().

next_step(#{nodes := Nodes, curr_step := Curr} = MarkovChain) ->
    CurrNode = maps:get(Curr, Nodes),
    NewNode = markov_node:get_random(CurrNode),
    case maps:is_key(NewNode, Nodes) of
        true ->
            MarkovChain#{curr_step => NewNode};
        false ->
            {error, node_undefined} % Maybe it's better to call error(node_undefined)
    end.

-spec curr_step(markov_chain(T)) ->
    T.

curr_step(#{curr_step := Curr}) ->
    Curr.
