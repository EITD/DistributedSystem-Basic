-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() -> [].

update(Node, Links, Map) ->
  [{Node, Links} | lists:keydelete(Node, 1, Map)].

reachable(Node, Map) ->
  case lists:keyfind(Node, 1, Map) of
    {Node, Links} -> Links;
    false -> []
  end.

all_nodes(Map) ->
%%  Returns a list containing the sorted
%% elements except the first element of the
%% elements comparing equal have been deleted.
  lists:usort(lists:foldl(fun({Node, Links}, All) ->
    [Node | Links ++ All] end, [], Map)).
