-module(library_chatlib).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([is_in_list/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_in_list(Item, List) ->
    case [Elem || Elem <- List, Elem == Item] of
        [] ->
            false;
        _ ->
            true
    end.
