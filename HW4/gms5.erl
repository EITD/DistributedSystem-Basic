-module(gms5).
-export([start/1, start/2]).
-define(timeout, 2000).
-define(arghh, 1000).
-define(arghh2, 100).

start(Id) ->
  Rnd = random:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun() -> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  leader(Id, Master, 0, [], [Master]).

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
    {view, N, [Leader | Slaves], Group} = Msg ->
      Leader ! {acc, Msg},
      Master ! {view, Group},
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, N + 1,
        {view, N, [Leader | Slaves], Group},
        Slaves, Group)
  after ?timeout ->
    Master ! {error, "no reply from leader"}
  end.

leader(Id, Master, N, Slaves, Group) ->
  receive
    {mcast, Msg} ->
%%      io:format("leader ~w send msg:~w~n", [Id, Msg]),
      bcast(Id, {msg, N, Msg}, Slaves),
      Master ! Msg,
      leader(Id, Master, N + 1, Slaves, Group);
    {join, Wrk, Peer} ->
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, N, [self() | Slaves2], Group2}, Slaves2),
      Master ! {view, Group2},
      leader(Id, Master, N + 1, Slaves2, Group2);
    stop -> ok
  end.

bcast(Id, Msg, Slaves) ->
  lists:foreach(fun(Slave) ->
    crashSend(Id, Slave, Msg),
    crash(Id) end, Slaves),
  acknowledgeMessage(Id, Msg, Slaves, length(Slaves)).

crashSend(Id, Slave, Msg) ->
  case random:uniform(?arghh2) of
    ?arghh2 ->
      io:format("leader ~w dropped message: ~w~n", [Id, Msg]);
    _ ->
      Slave ! Msg
  end.

crash(Id) ->
  case random:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~w: crash~n", [Id]),
      exit(no_luck);
    _ -> ok
  end.

acknowledgeMessage(_Id, Msg, _Slaves, 0) ->
%%  io:format("All Acc: ~w~n", [Msg]),
  ok;
acknowledgeMessage(Id, Msg, Slaves, NumAcc) ->
%%  io:format("Message: ~w NotAcc: ~w~n", [Msg, NumAcc]),
  receive
    {acc, Msg} ->
      acknowledgeMessage(Id, Msg, Slaves, NumAcc - 1)
  after 1000 ->
    io:format("Message ~w recast~n", [Msg]),
    bcast(Id, Msg, Slaves)
  end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {join, Wrk, Peer} ->
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, I, _} = NewLast when I < N ->
      Leader ! {acc, NewLast},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, I, Msg} = NewLast ->
%%      io:format("slave ~w recv msg:~w~n", [Id, Msg]),
      Leader ! {acc, NewLast},
      Master ! Msg,
      slave(Id, Master, Leader, I + 1, NewLast, Slaves, Group);
    {view, I, _, _} = NewLast when I < N ->
      Leader ! {acc, NewLast},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {view, I, [Leader | Slaves2], Group2} = NewLast ->
      Leader ! {acc, NewLast},
      Master ! {view, Group2},
      slave(Id, Master, Leader, I + 1, NewLast, Slaves2, Group2);
    {'DOWN', _Ref, process, Leader, _Reason} ->
      election(Id, Master, N, Last, Slaves, Group);
    stop ->
      ok
  end.

%% [_ | Group] delete the died leader's application layer process
election(Id, Master, N, Last, Slaves, [_ | Group]) ->
  Self = self(),
  case Slaves of
    [Self | Rest] ->
      io:format("Leader ~w~n", [Master]),
      bcast(Id, Last, Rest),
      bcast(Id, {view, N, Slaves, Group}, Rest),
      Master ! {view, Group},
      leader(Id, Master, N + 1, Rest, Group);
    [Leader | Rest] ->
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, N, Last, Rest, Group)
  end.