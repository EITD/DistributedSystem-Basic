-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).


%% Time - [{Node, Time}...]
%% Clock - [{Node, [{Node, Time}...]}...]

zero() -> [].

inc(Name, Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, T} -> lists:keyreplace(Name, 1, Time, {Name, T + 1});
    false -> [{Name, 1} | Time]
  end.

merge([], Time) ->
  Time;
merge([{Name, Ti} | Rest], Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Tj} ->
      [{Name, lists:max([Ti, Tj])} |
        merge(Rest, lists:keydelete(Name, 1, Time))];
    false -> [{Name, Ti} | merge(Rest, Time)]
  end.

leq([], _) ->
  true;
leq([{Name, Ti} | Rest], Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Tj} ->
      if
        Ti =< Tj ->
          leq(Rest, Time);
        true -> false
      end;
    false -> false
  end.

clock(Nodes) ->
%%  Node is taken from the Nodes
  [{Node, zero()} || Node <- Nodes].

update(From, Time, Clock) ->
  lists:keyreplace(From, 1, Clock, {From, Time}).

safe(Time, Clock) ->
%%  Returns true if fun returns true for all elements in Clock.
  lists:all(fun({_, T}) -> leq(Time, T) end, Clock).