-module(rudyImproved).
-export([init/1, handler/1, request/1, reply/1, start/1, stop/0]).

init(Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      handler(Listen),
      gen_tcp:close(Listen),
      ok;
    {error, Error} ->
      error
  end.

handler(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      spawn(fun() -> request(Client) end),
      handler(Listen);
    {error, Error} ->
      error
  end.

request(Client) ->
  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} ->
      [Method, Resource | _] = string:tokens(Str, " "),
      Path = [".", Resource],
%%	    Request = http:parse_request(Str),
%%		  Response = reply(Request),
      Response = get_file(Path),
      gen_tcp:send(Client, Response);
    {error, Error} ->
      io:format("rudy: error: ~w~n", [Error])
  end,
  gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
%%  timer:sleep(40),
  http:ok(URI).

get_file(Path) ->
  case file:read_file(Path) of
    {ok, Data} -> Response = response(Data);
    {error, _} -> Response = http:ok(Path)
  end,
  Response.

response(Data) ->
  io_lib:fwrite(
    "HTTP/1.0 ~s\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
    ["200 OK", size(Data), Data]).

start(Port) ->
  register(rudyImproved, spawn(fun() -> init(Port) end)).

stop() ->
  exit(whereis(rudyImproved), "time to die").
				
