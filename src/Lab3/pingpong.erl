%%%-------------------------------------------------------------------
%%% @author tkarkocha
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. kwi 2018 14:07
%%%-------------------------------------------------------------------
-module(pingpong).
-author("tkarkocha").

%% API
-export([start/0, stop/0, play/1]).

start() ->
  register(ping, spawn(fun() -> initPing() end)),
  register(pong, spawn(fun() -> initPong() end)).

stop() ->
  ping ! stop,
  pong ! stop.

play(N) ->
  ping ! N.

initPing() ->
  ping_loop().

initPong() ->
  pong_loop().

ping_loop() ->
  receive
    0 ->
      ping_loop();
    stop ->
      terminate();
    N ->
      io:format("ping ~w~n", [N]),
      timer:sleep(1500),
      pong ! (N-1),
      ping_loop()
  after
   20000 ->
     terminate()
  end.

pong_loop() ->
  receive
    0 ->
      pong_loop();
    stop ->
      terminate();
    N ->
      io:format("pong ~w~n", [N]),
      timer:sleep(1500),
      ping ! (N-1),
      pong_loop()

  after
    20000 ->
      terminate()
  end.

terminate() -> ok.


%inna funkcja
startA() ->
  register(ping, spawn(fun() -> bounce() end)),
  register(pong, spawn(fun() -> bounce() end)).

stopA() ->
  ping ! stop,
  pong ! stop.

playA(N) ->
  ping ! {N, pong}.

bounce() ->
  receive
    {N, pong} when N > 0 ->
      io:format("ping ~w~n", [N]),
      timer:sleep(2000),
      pong ! {N-1, ping},
      bounce();
    {N, ping} when N > 0 ->
      io:format("pong ~w~n", [N]),
      timer:sleep(2000),
      ping ! {N-1, pong},
      bounce();
    stop ->
      ok;
    _ ->
      bounce()
  after
    20000 -> ok
  end.

