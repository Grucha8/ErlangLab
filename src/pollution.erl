%%%-------------------------------------------------------------------
%%% @author tkarkocha
%%% @copyright (C) 2018, <COMPANY>
%%% @doc Module to monitor the pollution of air. Struct to store date is:
%%% #{ {StationName, {Xcord, Ycord}} => #{ {Date, Type} => Value } }
%%% Date = {Y, M, D, H}
%%%
%%% @end
%%% Created : 22. kwi 2018 23:13
%%%-------------------------------------------------------------------
-module(pollution).
-author("tkarkocha").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4]).
-export([getStationMean/3, getDailyMean/3, getHourlyStationData/3]).


-define(DATE, {{Year, Month, Day}, {Hour, _, _}}).

%% @doc Function to create a monitor which will
%% monitor the pollution stations
createMonitor() ->
  #{}.

%% @doc Update monitor M with new station named StationName
%% and with X and Y coords
addStation(StationName, {X, Y}, M) ->
  case checkKey(maps:keys(M), StationName, {X, Y}) of
    true -> io:format("This station already exists!~n"), M;
    false -> M#{{StationName, {X,Y}} => #{}}
  end.

%% @doc Updates value associated to Id in monitor M with
%% a new reading with Date, Type and Value
addValue(Id, ?DATE, Type, Value, M) ->
  FullId = retFullId(Id, M),
  Date = {Year, Month, Day, Hour},
  case checkKey(maps:keys(M), Id) and                         %if exits this station
       ( not checkKey(maps:keys(maps:get(FullId, M)), {Date, Type}) ) of
    false -> io:format("This record cant be added"), M;
    true  -> M#{ FullId => maps:put({Date, Type}, Value, maps:get(FullId, M)) }
  end.

%% @doc Removes reading {Date, Type} from station Id from monitor M
removeValue(Id, ?DATE, Type, M) ->
  Date = {Year, Month, Day, Hour},
  FullId = retFullId(Id, M),
  M#{ FullId => maps:remove({Date, Type}, maps:get(FullId, M)) }.

%% @doc Returns value associated with the station Id and the key {Date, Type}
%% from M monitor
getOneValue(Id, ?DATE, Type, M) ->
  Date = {Year, Month, Day, Hour},
  FullId = retFullId(Id, M),
  maps:get({Date, Type}, maps:get(FullId, M)).

%% @doc Returns an average from station Id with the type Type
getStationMean(Id, Type, M) ->
  FullId = retFullId(Id, M),
  {Sum, I, _} = maps:fold(fun valuesFun/3, {0, 0, Type}, maps:get(FullId, M)),
  Sum / I.

valuesFun({_, Type}, V, {Acc, I, PrimType}) when Type == PrimType ->
  {Acc + V, I + 1, PrimType};

valuesFun(_, _, {Acc, I, PrimType}) -> {Acc, I, PrimType}.

%% @doc Returns an average from all stations with the
%% same Type and Day
getDailyMean(Type, Day, M) ->
  {Sum, I, _} = maps:fold(fun dailyMean/3, {0, 0, {Day, Type}}, M),
  Sum / I.

dailyMean({{_,_,D,_}, Type}, V, {Acc, I, {Day, Type_}})
  when (D == Day) and (Type == Type_) ->
  {Acc + V, I + 1, {Day, Type_}};

dailyMean(_, V, {Acc, I, W}) when is_map(V) ->
  maps:fold(fun dailyMean/3, {Acc, I, W}, V);

dailyMean(_, _, {Acc, I, W}) when I > 0 -> {Acc, I, W};

dailyMean(_, _, {Acc, I, W}) -> error("There isnt ...").

%% @doc ?-?
getHourlyStationData(Id, Type, M) when is_list(Type) ->
  FullId = retFullId(Id, M),
  NewMap = getHourlyStationData(Type, maps:to_list(maps:get(FullId, M)), #{}),
  maps:to_list(maps:map(fun(K, {Sum, I}) -> (Sum / I) end, NewMap));

getHourlyStationData(_, [], M) -> M;

getHourlyStationData(Type, [{{{_,_,_,H}, Type_}, Value} | T], M) when Type == Type_ ->
  NewMap = maps:update_with(H,
    fun({Sum, I}) -> {Sum + Value, I + 1} end,
    {H, 1},
    M),
  getHourlyStationData(Type, T, NewMap);

getHourlyStationData(Type, [_ | T], M) -> getHourlyStationData(Type, T, M).


%% =================================================== %%
%%                   HELPING FUNCTIONS                 %%
%% =================================================== %%
retFullId(Name, M) when is_map(M) -> retFullId(Name, maps:keys(M));

retFullId(Name, [{N, C} | T]) when Name == N -> {N, C};

retFullId(Coord, [{N, C} | T]) when Coord == C -> {N, C};

retFullId(Name, [H | T]) -> retFullId(Name, T);

retFullId(_, []) -> error("Not like this").

checkKey([], _, _) ->
  false;

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
