-module(storage).
-export([create/0, add/3, lookup/2, split/3, merge/2]).

create() -> [].

%% the first occurrence of a tuple T whose Nth element compares equal to Key is replaced with NewTuple;
%% If there is no such tuple T, NewTuple has been appended
add(Key, Value, Store) ->
  lists:keystore(Key, 1, Store, {Key, Value}).

lookup(Key, Store) ->
  lists:keyfind(Key, 1, Store).

%% Partitions List into two lists, according to Pred(Elem) returns
split(From, To, Store) ->
  lists:partition(fun({Key, _}) ->
    key:between(Key, From, To) end, Store).

merge(Entries, Store) ->
  lists:merge(Entries, Store).