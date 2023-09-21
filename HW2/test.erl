-module(test).
-export([start/0, broadcast/0, update/0, stop/0]).

%% erl -name sweden@130.229.179.239 -setcookie routy -connect_all false
start() ->
  routy:start(stockholm, stockholm),
  routy:start(lund, lund),
  routy:start(helsingborg, helsingborg),
  routy:start(uppsala, uppsala),
  routy:start(gothenburg, gothenburg),
  stockholm ! {add, lund, {lund, 'sweden@130.229.179.239'}},
  lund ! {add, helsingborg, {helsingborg, 'sweden@130.229.179.239'}},
  helsingborg ! {add, uppsala, {uppsala, 'sweden@130.229.179.239'}},
  uppsala ! {add, gothenburg, {gothenburg, 'sweden@130.229.179.239'}},
  gothenburg ! {add, stockholm, {stockholm, 'sweden@130.229.179.239'}},
%%  stockholm ! {add, gothenburg, {gothenburg, 'sweden@130.229.179.239'}},
%%  gothenburg ! {add, uppsala, {uppsala, 'sweden@130.229.179.239'}},
%%  uppsala ! {add, helsingborg, {helsingborg, 'sweden@130.229.179.239'}},
%%  helsingborg ! {add, lund, {lund, 'sweden@130.229.179.239'}},
%%  lund ! {add, stockholm, {stockholm, 'sweden@130.229.179.239'}},
  starttrue.

broadcast() ->
  stockholm ! broadcast,
  lund ! broadcast,
  helsingborg ! broadcast,
  uppsala ! broadcast,
  gothenburg ! broadcast,
  broadcasttrue.

update() ->
  stockholm ! update,
  lund ! update,
  helsingborg ! update,
  uppsala ! update,
  gothenburg ! update,
  updatetrue.

stop() ->
  routy:stop(stockholm),
  routy:stop(lund),
  routy:stop(helsingborg),
  routy:stop(uppsala),
  routy:stop(gothenburg),
  stoptrue.