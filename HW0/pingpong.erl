-module(pingpong).
-export([start_pong/0, start_ping/1, ping/1, pong/0]).

ping(Pong_Node) ->
    {pong, Pong_Node} ! {ping, self()},
    receive
        pong ->
             io:format("Ping received pong~n", [])
    end.

pong() ->
    receive
       {ping, Ping_Pid} ->
            io:format("Pong received ping~n", []),
            Ping_Pid ! pong
    end.

start_pong() ->
    register(pong, spawn(pingpong, pong, [])).

start_ping(Pong_Node) ->
    spawn(pingpong, ping, [Pong_Node]).