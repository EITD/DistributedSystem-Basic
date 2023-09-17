-module(dijkstra).
-export([table/2, route/2]).

%% Sorted - {City, Dist, Gateway}
%% Map - {City, [Links]}
%% Table - [{City, Gateway}]

entry(Node, Sorted) ->
  case lists:keyfind(Node, 1, Sorted) of
    false -> 0;
    {_, Dist, _} -> Dist
  end.

replace(Node, N, Gateway, Sorted) ->
%%  Returns the sorted list formed by merging
%% TupleList1 and TupleList2. The merge is
%% performed on the Nth element of each tuple.
  lists:keymerge(2, [{Node, N, Gateway}],
    lists:keydelete(Node, 1, Sorted)).

update(Node, N, Gateway, Sorted) ->
  Dist = entry(Node, Sorted),
  if
    Dist > N -> replace(Node, N, Gateway, Sorted);
    true -> Sorted
  end.

iterate(Sorted, Map, Table) ->
  case Sorted of
    [] -> Table;
    [{_, inf, _} | _] -> Table;
    [{Node, Dist, Gateway} | Rest] ->
%%      apply update to each element in map:reachable,
%%      Sorted is also updated through S,
%%      update starts from Rest
      NewSorted = lists:foldl(fun(N, S) ->
        update(N, Dist + 1, Gateway, S) end, Rest,
        map:reachable(Node, Map)),
      iterate(NewSorted, Map,
        [{Node, Gateway} | Table])
  end.

%% construct a table with {city, gateway}
table(Gateways, Map) ->
%%  city-not gateway
  C = lists:map(fun(N) -> {N, inf, unknown} end,
    lists:filter(fun(N) -> not
%%    Returns true if Elem matches some
%%    element of List, otherwise false.
    lists:member(N, Gateways) end,
      map:all_nodes(Map))),
%%  gateway
  G = lists:map(fun(N) -> {N, 0, N} end, Gateways),
  iterate(G ++ C, Map, []).

route(Node, Table) ->
  case lists:keyfind(Node, 1, Table) of
    {_, Gateway} -> {ok, Gateway};
    false -> notfound
  end.