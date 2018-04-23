%%%-------------------------------------------------------------------
%%% @author tkarkocha
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. kwi 2018 11:33
%%%-------------------------------------------------------------------
-module(qsort).
-author("tkarkocha").

%% API
-export([qs/1, compareSpeeds/3, randomElems/3, qsBetter/1, myMap/2]).

%funkce pomocnicze
lessThan(List, Arg) ->
  [X || X <- List, X < Arg].

grtEqThan(List, Arg) ->
  [X || X <- List, X >= Arg].

%quicksort "slaby"
qs([Pivot | Tail]) ->
  qs( lessThan(Tail, Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail, Pivot));

qs([]) ->
  [].

%generacja listy losowych l=elementow
randomElems(N, Min, Max) ->
  [(rand:uniform(Max - Min) + Min) || X <- lists:seq(1, N)].

compareSpeeds(List, Fun1, Fun2) ->
  {T1, _} = timer:tc(Fun1, [List]),
  {T2, _} = timer:tc(Fun2, [List]),
  io:format("czas 1 = ~w~ncasz 2 = ~w~n", [T1, T2]).

%qucik sort na foldzie
qsBetter(List, Arg) ->
  lists:foldl(fun myFilter/2, {[], [], Arg}, List).

qsBetter([]) ->
  [];

qsBetter([H | T]) ->
  {Lt, Gt, _} = qsBetter(T, H),
  qsBetter(Lt) ++ [H] ++ qsBetter(Gt).

myFilter(Arg, {Lt, Gt, Pom} ) when Arg < Pom ->
  {[Arg | Lt], Gt, Pom};

myFilter(Arg, {Lt, Gt, Pom} ) when Arg >= Pom ->
  {Lt, [Arg | Gt], Pom}.

%Moja definicja mapy
myMap(Fun, List) ->
  [Fun(X) || X <- List].

