-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).


zero() -> 0.

inc(Name, T) -> T + 1.

merge(Ti, Tj) ->
  if Ti < Tj -> Tj;
    true -> Ti
  end.

leq(Ti, Tj) ->
  if Ti > Tj -> false;
    true -> true
  end.

clock(Nodes) ->
%%  Node is taken from the Nodes
  [{Node, zero()} || Node <- Nodes].

update(Node, Time, Clock) ->
  lists:keyreplace(Node, 1, Clock, {Node, Time}).

safe(Time, Clock) ->
%%  Returns true if fun returns true for all elements in Clock.
  lists:all(fun({_, T}) -> leq(Time, T) end, Clock).