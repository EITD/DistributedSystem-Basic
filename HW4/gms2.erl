-module(gms2).
-export([start/1, start/2]).
-define(timeout, 2000).
-define(arghh, 1000).

start(Id) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun() -> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  leader(Id, Master, [], [Master]).

%% Grp: One application layer process it made a join request to.
start(Id, Grp) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun() -> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    {view, [Leader | Slaves], Group} ->
      Master ! {view, Group},
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, Slaves, Group)
  after ?timeout ->
    Master ! {error, "no reply from leader"}
  end.

leader(Id, Master, Slaves, Group) ->
  receive
    {mcast, Msg} ->
%%      io:format("leader ~w send msg:~w~n", [Id, Msg]),
      bcast(Id, {msg, Msg}, Slaves),
      Master ! Msg,
      leader(Id, Master, Slaves, Group);
    {join, Wrk, Peer} ->
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, [self() | Slaves2], Group2}, Slaves2),
      Master ! {view, Group2},
      leader(Id, Master, Slaves2, Group2);
    stop -> ok
  end.

bcast(Id, Msg, Slaves) ->
  lists:foreach(fun(Slave) -> Slave ! Msg, crash(Id) end, Slaves).

crash(Id) ->
  case random:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~w: crash~n", [Id]),
      exit(no_luck);
    _ -> ok
  end.

slave(Id, Master, Leader, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, Slaves, Group);
    {join, Wrk, Peer} ->
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, Slaves, Group);
    {msg, Msg} ->
%%      io:format("slave ~w recv msg:~w~n", [Id, Msg]),
      Master ! Msg,
      slave(Id, Master, Leader, Slaves, Group);
    {view, [Leader | Slaves2], Group2} ->
      Master ! {view, Group2},
      slave(Id, Master, Leader, Slaves2, Group2);
    {'DOWN', _Ref, process, Leader, _Reason} ->
      election(Id, Master, Slaves, Group);
    stop ->
      ok
  end.

election(Id, Master, Slaves, [_ | Group]) ->
  Self = self(),
  case Slaves of
    [Self | Rest] ->
      io:format("Leader ~w~n", [Master]),
      bcast(Id, {view, Slaves, Group}, Rest),
      Master ! {view, Group},
      leader(Id, Master, Rest, Group);
    [Leader | Rest] ->
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, Rest, Group)
  end.