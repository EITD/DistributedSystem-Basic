-module(test2).
-compile(export_all).

%one node only in the ring and let the four test machines
%add N elements to the ring and then do a lookup of the
%elements
performance1(N) ->
  Node = test:start(node2),
  %wait for stabilization of ring
  timer:sleep(1000),
  spawn(fun() -> add_lookup(1, N, Node) end),
  spawn(fun() -> add_lookup(2, N, Node) end),
  spawn(fun() -> add_lookup(3, N, Node) end),
  spawn(fun() -> add_lookup(4, N, Node) end),
  Node.

%one machine to handle 4000 elements
performance2(N) ->
  Node = test:start(node2),
  timer:sleep(1000),
  spawn(fun() -> add_lookup(1, N, Node) end),
  Node.

% test Machine add N elements into one known node
add_lookup(Id, N, P) ->
  T1 = now(),
  io:format("Test Machine~w:~w add ~w elements~n", [Id, self(), N]),
  Keys = test:keys(N),
  test:add(Keys, P),
  test:check(Keys, P, 0, 0),
  T2 = now(),
  Done = (timer:now_diff(T2, T1) div 1000),
  io:format("Test Machine~w: finish in ~w ms ~n", [Id, Done]).