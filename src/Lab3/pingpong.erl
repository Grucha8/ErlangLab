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
  register(ping, spawn(fun() -> bounce() end)),
  register(pong, spawn(fun() -> bounce() end)).

stop() ->
  ping ! stop,
  pong ! stop.

play(N) ->
  ping ! {N, pong}.

bounce() ->
  receive
    {N, pong} when N > 0 ->
      io:format("ping~n"),
      timer:sleep(2000),
      pong ! {N-1, ping},
      bounce();
    {N, ping} when N > 0 ->
      io:format("pong~n"),
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



