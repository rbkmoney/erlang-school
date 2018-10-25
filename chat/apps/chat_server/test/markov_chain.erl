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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec create(NodesMap :: node_map(T), InitialNode :: markov_node:markov_node(T)) ->
    markov_chain(T).

create(NodesMap, InitialNode) ->
    ok = check_correctness(NodesMap),
    case maps:is_key(InitialNode, NodesMap) of
        true ->
            #{nodes => NodesMap, curr_step => InitialNode};
        false ->
            error(invalid_node_map)
    end.

-spec next_step(markov_chain(T)) ->
    markov_chain(T).

next_step(#{nodes := Nodes, curr_step := Curr} = MarkovChain) ->
    CurrNode = maps:get(Curr, Nodes),
    NewNode = markov_node:get_random(CurrNode),
    MarkovChain#{curr_step => NewNode}.

-spec curr_step(markov_chain(T)) ->
    T.

curr_step(#{curr_step := Curr}) ->
    Curr.

-spec check_correctness(node_map(_)) ->
    ok | no_return().

check_correctness(NodeMap) -> % Пока совершено монструозная конструкция, подумаю над тем, как улучшить ее
    % Если входные данные невалидны, эта функция просто кинет ошибку
    Nodes = maps:values(NodeMap),
    AllKeys = lists:umerge(lists:map(fun maps:values/1, Nodes)),
    UniqueKeys = sets:to_list(sets:from_list(AllKeys)),
    F = fun(Item, Map) ->
        case maps:is_key(Item, Map) of
            true ->
                Map;
            false ->
                error(invalid_node_map)
        end
    end,
    lists:foldl(F, NodeMap, UniqueKeys),
    ok.
