-module(node4).
-export([start/1, start/2]).
-define(Stabilize, 100).
-define(Timeout, 10000).


start(Id) ->
  start(Id, nil).
start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  Predecessor = nil,
  {ok, {Skey, Spid}} = connect(Id, Peer),
%%  monitor1
  Sref = monitor(Spid),
  schedule_stabilize(),
  node(Id, Predecessor, {Skey, Sref, Spid},
    storage:create(), nil, storage:create()).

connect(Id, nil) ->
  {ok, {Id, self()}};
connect(Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey, Peer}}
  after ?Timeout ->
    io:format("~w Time out: no response~n", [Id])
  end.

node(Id, Predecessor, Successor, Store, Next, Replica) ->
  receive
%%    a peer needs to know our key
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store, Next, Replica);
%%    a new node informs us of its existence
    {notify, New} ->
      {Pred, NewStore} = notify(New, Id, Predecessor, Store),
      {_, _, Spid} = Successor,
%%    our store updated, notify successor's replica
      Spid ! {replica, NewStore},
      node(Id, Pred, Successor, NewStore, Next, Replica);
%%    a predecessor needs to know our predecessor and Next
    {request, Peer} ->
      request(Peer, Predecessor, Successor),
      node(Id, Predecessor, Successor, Store, Next, Replica);
%%    our successor informs us about its predecessor and Next
    {status, Pred, Nx} ->
      {Succ, Nxt} = stabilize(Pred, Id, Successor, Nx),
      node(Id, Predecessor, Succ, Store, Nxt, Replica);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store, Next, Replica);

    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client,
        Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added, Next, Replica);
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store, Next, Replica);
%%    takeover part of the store and part of the replica
    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
%%    our store updated, notify successor's replica
      {_, _, Spid} = Successor,
      Spid ! {replica, Merged},
      node(Id, Predecessor, Successor, Merged, Next, Replica);
    {replicate, Key, Value} ->
      node(Id, Predecessor, Successor, Store, Next, storage:add(Key, Value, Replica));
%%    update the Replica
    {replica, Repl} ->
      node(Id, Predecessor, Successor, Store, Next, Repl);

    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store, Next, Replica);
%%    the probe returns
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store, Next, Replica);
%%    not our probe
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store, Next, Replica);

    {kill, Id} ->
      exit(-1);
    {kill, Other} ->
      {_, _, Spid} = Successor,
      Spid ! {kill, Other},
      node(Id, Predecessor, Successor, Store, Next, Replica);
%%    should update store and replica
    {'DOWN', Ref, process, _, _} ->
      {Pred, Succ, Nxt, Sto, Repl} = down(Ref, Predecessor, Successor, Next, Store, Replica),
      node(Id, Pred, Succ, Sto, Nxt, Repl);
    node_status ->
      io:format("~w Pre: ~w, Suc: ~w, Next: ~w, Store: ~w, Replica: ~w~n", [Id, Predecessor, Successor, Next, Store, Replica]),
      node(Id, Predecessor, Successor, Store, Next, Replica)
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

%% send a request message to our successor.
stabilize({_, _, Spid}) ->
  Spid ! {request, self()}.

%% inform the peer about our predecessor and Next
request(Peer, Predecessor, {Skey, _, Spid}) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil, {Skey, Spid}};
    {Pkey, _, Ppid} ->
      Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
  end.

%%  Pred is our successor’s current predecessor
stabilize(Pred, Id, Successor, Next) ->
  {Skey, Sref, Spid} = Successor,
  case Pred of
%%    our successor's predecessor is nil
    nil -> Spid ! {notify, {Id, self()}},
      {Successor, Next};
%%    our successor points back to us
    {Id, _} -> {Successor, Next};
%%    our successor points to itself
    {Skey, _} -> Spid ! {notify, {Id, self()}},
      {Successor, Next};
%%    our successor points to another node
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
%%    our successor's predecessor's key is between us and its successor
%%    de-monitor1 && monitor2
        true ->
          drop(Sref),
          NewRef = monitor(Xpid),
          {{Xkey, NewRef, Xpid}, {Skey, Spid}};
%%    our key is between our successor's predecessor and our successor
        false -> Spid ! {notify, {Id, self()}},
          {Successor, Next}
      end
  end.

%% check new node and return predecessor
notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
%%      monitor3
      NRef = monitor(Npid),
      {{Nkey, NRef, Npid}, Keep};
    {Pkey, Pref, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Nkey, Npid),
%%       de-monitor2 && monitor4
          drop(Pref),
          NRef = monitor(Npid),
          {{Nkey, NRef, Npid}, Keep};
%%      ignore new node's request
        false -> {Predecessor, Store}
      end
  end.

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Client ! {Qref, ok},
      Spid ! {replicate, Key, Value},
      storage:add(Key, Value, Store);
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.

lookup(Key, Qref, Client, Id, {Pkey, _, _}, Successor, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      {_, _, Spid} = Successor,
      Spid ! {lookup, Key, Qref, Client}
  end.

handover(Id, Store, Nkey, Npid) ->
%%  (NewPkey, Id] return, (OldPkey, NewPkey) belongs to NewNode
  {Keep, Rest} = storage:split(Nkey, Id, Store),
  Npid ! {handover, Rest},
  Keep.

%% send a probe to check if the ring is actually connected
create_probe(Id, {_, _, Spid}) ->
  Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

%% report the time it took to pass around the ring
remove_probe(T, Nodes) ->
  io:format("Probe: ~wµs ~n Nodes: ~w~n",
    [erlang:system_time(micro_seconds) - T,
      lists:reverse(Nodes)]).

%% forward it to our successor and add our own pid to the list of nodes
forward_probe(Ref, T, Nodes, Id, {_, _, Spid}) ->
  io:format("~w forward probe~n", [Id]),
  Spid ! {probe, Ref, [Id | Nodes], T}.

monitor(Pid) ->
  erlang:monitor(process, Pid).

drop(nil) ->
  ok;
drop(Pid) ->
  erlang:demonitor(Pid, [flush]).

down(Ref, {_, Ref, _}, Successor, Next, Store, Replica) ->
  {nil, Successor, Next, storage:merge(Store, Replica), storage:create()};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}, Store, Replica) ->
  Nref = monitor(Npid),
  stabilize({Nkey, Nref, Npid}),
  {Predecessor, {Nkey, Nref, Npid}, nil, Store, Replica}.
