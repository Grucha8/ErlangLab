%%%-------------------------------------------------------------------
%%% @author tkarkocha
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. mar 2018 13:40
%%%-------------------------------------------------------------------
-module(myList).
-author("tkarkocha").

%% API
-export([contains/2, duplicateElements/1, sumFloatsRec/1, sumFloatsTail/1]).

contains([], _) ->
  false;

contains([H | T], Value) ->
  contains(T, Value) or (H == Value).

duplicateElements([]) ->
  [];

duplicateElements([H | T]) ->
  [H, H | duplicateElements(T)].

sumFloatsRec([]) ->
  0;

sumFloatsRec([H | T]) when is_float(H) ->
  H + sumFloatsRec(T);

sumFloatsRec([H | T]) ->
  0 + sumFloatsRec(T).

sumFloatsTail(L) ->
  sumFloatsTail(L, 0).

sumFloatsTail([], Sum) ->
  Sum;

sumFloatsTail([H | T], Sum) when is_float(H) ->
  sumFloatsTail(T, Sum + H);

sumFloatsTail([H | T], Sum) ->
  sumFloatsTail(T, Sum).