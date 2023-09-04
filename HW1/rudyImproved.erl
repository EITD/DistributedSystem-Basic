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
	    Request = http:parse_request(Str),
		  Response = reply(Request),
	    gen_tcp:send(Client, Response);
	{error, Error}  ->
	    io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
	  timer:sleep(40),
    http:ok(URI).

start(Port) ->
    register(rudyImproved, spawn(fun() -> init(Port) end)).

stop() ->
    exit(whereis(rudyImproved), "time to die").
				