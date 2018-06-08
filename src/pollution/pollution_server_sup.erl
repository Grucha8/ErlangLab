%%%-------------------------------------------------------------------
%%% @author tkarkocha
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. maj 2018 11:32
%%%-------------------------------------------------------------------
-module(pollution_server_sup).
-author("tkarkocha").

%% API
-export([start/0, init/0, stop/0]).

start() ->
  register(superviser, spawn(fun() -> init() end)).

init() ->
  process_flag(trap_exit, true),
  loop().


loop() ->
  pollution_server:start(),
  receive
    {'EXIT', Pid, Reason} ->
      io:format("Pollution server shutdown because: ~p~nResuming~n", [Reason]),
      loop();
    stop ->
      ok
  end.


stop() -> superviser ! stop.