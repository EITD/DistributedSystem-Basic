-module(test2).
-export([start/0, broadcast/0, update/0, stop/0]).

%% erl -name sweden@130.229.186.138 -setcookie routy -connect_all false
start() ->
  routy:start(paris, paris),
  routy:start(lyon, lyon),
  routy:start(masai, masai),
  routy:start(provence, provence),
  routy:start(bordeaux, bordeaux),
  paris ! {add, lyon, {lyon, 'sweden@130.229.186.138'}},
  lyon ! {add, masai, {masai, 'sweden@130.229.186.138'}},
  masai ! {add, provence, {provence, 'sweden@130.229.186.138'}},
  provence ! {add, bordeaux, {bordeaux, 'sweden@130.229.186.138'}},
  bordeaux ! {add, paris, {paris, 'sweden@130.229.186.138'}},
  starttrue.

broadcast() ->
  paris ! broadcast,
  lyon ! broadcast,
  masai ! broadcast,
  provence ! broadcast,
  bordeaux ! broadcast,
  broadcasttrue.

update() ->
  paris ! update,
  lyon ! update,
  masai ! update,
  provence ! update,
  bordeaux ! update,
  updatetrue.

stop() ->
  routy:stop(paris),
  routy:stop(lyon),
  routy:stop(masai),
  routy:stop(provence),
  routy:stop(bordeaux),
  stoptrue.