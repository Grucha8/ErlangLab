%%%-------------------------------------------------------------------
%%% @author tkarkocha
%%% @copyright (C) 2018, <COMPANY>
%%% @doc Module to monitor the pollution of air. Struct to store date is:
%%% #{ {StationName, {Xcord, Ycord}} => #{ {Date, Type} => Value } }
%%%
%%% @end
%%% Created : 22. kwi 2018 23:13
%%%-------------------------------------------------------------------
-module(pollution).
-author("tkarkocha").

%% API
-export([createMonitor/0, addStation/3]).

%% @doc Function to create a monitor which will
%% monitor the pollution stations
createMonitor() ->
  #{}.

%% @doc Function to add new station.
%% @param
addStation(StationName, {X, Y}, M) ->
  case checkKey(maps:keys(M), StationName, {X, Y}) of
    true -> io:format("This station already exists!"), M;
    false -> M#{{StationName, {X,Y}} => #{}}
  end.



%% @doc Function to add new reading to monitor
addValue(Id, Date, Type, Value, M) ->
  FullId = retFullId(Id, M),
  case checkKey(maps:keys(M), Id) and                         %if exits this station
       ( not checkKey(maps:keys(maps:get(FullId)), {Date, Type}) ) of
    false -> io:format("This record cant be added"), M;
    true  -> M#{ FullId => maps:put({Date, Type}, Value, maps:get(FullId, M)) }
  end.


retFullId(Coord, [{N, C} | T]) when Coord == C -> {N, C};

retFullId(Name, [{N, C} | T]) when Name == N -> {N, C};

retFullId(Name, [H | T]) -> returnOtherPartOfId(Name, T);

retFullId(_, []) -> error("Not like this").


%% @doc Helper function to find out if the Name and coords
%% are already in our Monitor.
%% Returns: true if is, false if isn't
checkKey([], _, _) ->
  false;

%% @todo change the names of params
checkKey([{N, Coords} | T], SN, C) when (N == SN) or (Coords == C) ->
  true;

checkKey([H | T], SN, C) ->
  false orelse checkKey(T, SN, C).

% Function checkKey for two args
checkKey([], _) ->
  false;

checkKey([{N, _} | T], SN) when SN == N ->
  true;

checkKey([{_, Cord} | T], {X, Y}) when Cord == {X, Y} ->
  true;

checkKey([H | T], Check) ->
  false orelse checkKey(T, Check).
