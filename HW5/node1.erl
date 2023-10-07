-module(node1).
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
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  node(Id, Predecessor, Successor).

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

node(Id, Predecessor, Successor) ->
  receive
%%    a peer needs to know our key
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor);
%%    a new node informs us of its existence
    {notify, New} ->
      Pred = notify(New, Id, Predecessor),
      node(Id, Pred, Successor);
%%    a predecessor needs to know our predecessor
    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor);
%%    our successor informs us about its predecessor
    {status, Pred} ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor);
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor);
%%    the probe returns
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor);
%%    not our probe
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor)
  end.

%% send a probe to check if the ring is actually connected
create_probe(Id, {_, Spid}) ->
  Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

%% report the time it took to pass around the ring
remove_probe(T, Nodes) ->
  io:format("Probe: ~wµs ~n Nodes: ~w~n",
    [erlang:system_time(micro_seconds) - T,
      lists:reverse(Nodes)]).

%% forward it to our successor and add our own pid to the list of nodes
forward_probe(Ref, T, Nodes, Id, {_, Spid}) ->
  io:format("~w forward probe~n", [Id]),
  Spid ! {probe, Ref, [Id | Nodes], T}.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

%% send a request message to our successor.
stabilize({_, Spid}) ->
  Spid ! {request, self()}.

%% inform the peer about our predecessor
request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.

%%  Pred is our successor’s current predecessor
stabilize(Pred, Id, Successor) ->
  {Skey, Spid} = Successor,
  case Pred of
%%    our successor's predecessor is nil
    nil -> Spid ! {notify, {Id, self()}},
      Successor;
%%    our successor points back to us
    {Id, _} -> Successor;
%%    our successor points to itself
    {Skey, _} -> Spid ! {notify, {Id, self()}},
      Successor;
%%    our successor points to another node
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
%%    our successor's predecessor's key is between us and its successor
        true -> {Xkey, Xpid};
%%    our key is between our successor's predecessor and our successor
        false -> Spid ! {notify, {Id, self()}},
          Successor
      end
  end.

%% check new node and return predecessor
notify({Nkey, Npid}, Id, Predecessor) ->
  case Predecessor of
    nil -> {Nkey, Npid};
    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true -> {Nkey, Npid};
%%      ignore new node's request and inform new node
        false -> Npid ! {status, Predecessor},
          Predecessor
      end
  end.
