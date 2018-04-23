%%%-------------------------------------------------------------------
%%% @author tkarkocha
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. mar 2018 13:26
%%%-------------------------------------------------------------------
-module(lab1).
-author("tkarkocha").

%% API
-export([powerRec/2, powerTail/2]).

powerTail(A, B) ->
  power(A, B, B).

power(A, B, N) when N > 1 ->
  power(A*A, B, N-1);

power(A, B, N) ->
  io:format("~B~n", [A]).

powerRec(A, B) when B < 0 ->
  io:format("Wykladnik nie moze byc ujemny");

powerRec(A, B) when B < 1 ->
  1;

powerRec(A, B) ->
  A * powerRec(A, B-1).