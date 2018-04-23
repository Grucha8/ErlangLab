%%%-------------------------------------------------------------------
%%% @author tkarkocha
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. mar 2018 13:58
%%%-------------------------------------------------------------------
-module(onp).
-author("tkarkocha").

%% API
-export([onp/1]).

onp(S) ->
  L = string:tokens(S, " "),
  onp(L, []).

onp(L, [H | T]) when length(L) == 0 ->
  H;

%rozpoznawanie operacjionp
onp([H | T], [A | ST]) when (H =:= "sqrt") ->
  onp(T, [math:sqrt(A) | ST]);

onp([H | T], [A | ST]) when (H =:= "sin") ->
  onp(T, [math:sin(A) | ST]);

onp([H | T], [A | ST]) when (H =:= "cos") ->
  onp(T, [math:cos(A) | ST]);

onp([H | T], [A | ST]) when (H =:= "tan") ->
  onp(T, [math:tan(A) | ST]);

onp([H | T], [A | [B | ST]]) when (H =:= "+") ->
  onp(T, [A+B | ST]);

onp([H | T], [A | [B | ST]]) when (H =:= "-") ->
  onp(T, [B-A | ST]);

onp([H | T], [A | [B | ST]]) when (H =:= "*") ->
  onp(T, [A*B | ST]);

onp([H | T], [A | [B | ST]]) when (H =:= "/") ->
  onp(T, [B/A | ST]);

onp([H | T], [A | [B | ST]]) when (H =:= "**") ->
  onp(T, [math:pow(B, A) | ST]);

%jesli H nie jest operatorem to jest liczba
onp([H | T], Stack) ->
  case lists:member(46, H) of            %46 == '.' sprawdzamy czy to float
    true  -> onp(T, [list_to_float(H) | Stack]);
    false -> onp(T, [list_to_integer(H) | Stack]);
    _     -> io:format("Zla liczba"), exit(1)
  end.





