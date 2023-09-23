-module(mylogger).
-export([start/1, stop/1]).


start(Nodes) ->
  spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  loop(time:clock(Nodes), []).

%%   every time put the element in the ordered place to reduce time complexity
insert({From, Time, Msg}, []) ->
  [{From, Time, Msg}];
insert({_, Time, _} = Message, [Head = {_, T, _} | Rest] = Queue) ->
  case time:leq(Time, T) of
    true ->
      [Message | Queue];
    false ->
      [Head | insert(Message, Rest)]
  end.

loop(Clock, Queue) ->
  receive
    {log, From, Time, Msg} ->
%%      io:format("realtime: ~w ~w ~p~n", [Time, From, Msg]),
      io:format("~w~n", [length(Queue)]),
      NewClock = time:update(From, Time, Clock),
%%      NewQueue = goThrough(NewClock, insert({From, Time, Msg}, Queue)),
%%      Removes elements from TempQueue that satisfy fun until the first element that does not satisfy.
      NewQueue = lists:dropwhile(fun({F, T, M}) ->
        case time:safe(T, NewClock) of
          true ->
            log(F, T, M),
            true;
          false ->
            false
        end
                                 end,
        insert({From, Time, Msg}, Queue)),
      loop(NewClock, NewQueue);
    stop -> ok
  end.

%%goThrough(_, []) ->
%%  [];
%%goThrough(Clock, [{From, Time, Msg} | Rest] = Queue) ->
%%  case time:safe(Time, Clock) of
%%    true -> log(From, Time, Msg),
%%      goThrough(Clock, Rest);
%%    %%  exit because queue is sorted
%%    false -> Queue
%%  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
