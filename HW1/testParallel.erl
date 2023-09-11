-module(testParallel).
-export([bench/3]).

bench(Host, Port, N) ->
  P = self(),
  Receiver = spawn(fun()->receiver(N,P) end),
  Start = erlang:system_time(micro_seconds),
  run(N, Receiver, Host, Port),
  receive
    all_done -> ok
  end,
  Finish = erlang:system_time(micro_seconds),
  Finish - Start.

receiver(N, Pid) ->
  if N == 0 ->
    Pid ! all_done;
    true ->
      receive
        done -> receiver(N-1,Pid);
        error -> receiver(N-1,Pid)
      end
  end.

run(N, Receiver, Host, Port) ->
  if
    N == 0 -> ok;
    true ->
      spawn(fun() -> request(Receiver, Host, Port) end),
      run(N - 1, Receiver, Host, Port)
  end.

request(Receiver, Host, Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:connect(Host, Port, Opt) of
    {ok, Server} ->
      gen_tcp:send(Server, http:get("foo")),
      Recv = gen_tcp:recv(Server, 0),
      case Recv of
        {ok, _} ->
          Receiver ! done;
        {error, Error} ->
          Receiver ! error
      end,
      gen_tcp:close(Server);
    {error, _} ->
      Receiver ! error
  end.
