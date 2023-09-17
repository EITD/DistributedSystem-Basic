-module(test).
-export([start/0, broadcast/0, update/0, stop/0]).

%% erl -name sweden@192.168.1.100 -setcookie routy -connect_all false
start() ->
  routy:start(r1, stockholm),
  routy:start(r2, lund),
  routy:start(r3, helsingborg),
  routy:start(r4, uppsala),
  routy:start(r5, gothenburg),
  r1 ! {add, lund, {r2, 'sweden@192.168.1.100'}},
  r2 ! {add, helsingborg, {r3, 'sweden@192.168.1.100'}},
  r3 ! {add, uppsala, {r4, 'sweden@192.168.1.100'}},
  r4 ! {add, gothenburg, {r5, 'sweden@192.168.1.100'}},
  r5 ! {add, stockholm, {r1, 'sweden@192.168.1.100'}},
  starttrue.

broadcast() ->
  r1 ! broadcast,
  r2 ! broadcast,
  r3 ! broadcast,
  r4 ! broadcast,
  r5 ! broadcast,
  broadcasttrue.

update() ->
  r1 ! update,
  r2 ! update,
  r3 ! update,
  r4 ! update,
  r5 ! update,
  updatetrue.

stop() ->
  routy:stop(r1),
  routy:stop(r2),
  routy:stop(r3),
  routy:stop(r4),
  routy:stop(r5),
  stoptrue.