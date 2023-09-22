-module(mylogger).
-export([start/1, stop/1]).


start(Nodes) ->
  spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  loop(time:clock(Nodes), []).

loop(Clock, Queue) ->
  receive
    {log, From, Time, Msg} ->
%%      io:format("realtime: ~w ~w ~p~n", [Time, From, Msg]),
      NewClock = time:update(From, Time, Clock),
%%    keymerge reduce time complexity
      NewQueue = lists:keymerge(2, [{From, Time, Msg}], Queue),
      case time:safe(Time, NewClock) of
        true ->
          UpdateQueue = goThrough(Time, NewQueue),
          loop(NewClock, UpdateQueue);
%%      exit
        false ->
          loop(NewClock, NewQueue)
      end;
    stop -> ok
  end.

%% at most update one clock time, only compare to the updated clock time
goThrough(_, []) ->
  [];
goThrough(Time, [{From, MsgTime, Msg} | Rest]) ->
  if
    MsgTime =< Time -> log(From, MsgTime, Msg),
      goThrough(Time, Rest);
%%  exit because queue is sorted
    true -> [{From, MsgTime, Msg} | Rest]
  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
