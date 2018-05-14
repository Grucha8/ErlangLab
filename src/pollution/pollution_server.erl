%%%-------------------------------------------------------------------
%%% @author tkarkocha
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. kwi 2018 12:40
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("tkarkocha").

%% API
-export([start/0, stop/0]).
-export([addStation/2, addValue/4, removeValue/4, getOneValue/3, getHourlyStationData/2]).
-export([getDailyMean/2, getStationMean/2, showMonitor/0]).


start() ->
  register(pollServer, spawn(fun() -> init() end)).


init() ->
  State = pollution:createMonitor(),
  loop(State).


loop(State) ->
  receive
    {request, Pid, {addStation, {Name, Coords}}} ->
      NewState = pollution:addStation(Name, Coords, State),
      case NewState of
        State -> Pid ! {reply, error};
        _     -> Pid ! {reply, ok}
      end,
      loop(NewState);
    {request, Pid, {addValue, {Id, Date, Type, Value}}} ->
      NewState = pollution:addValue(Id, Date, Type, Value, State),
      case NewState of
        State -> Pid ! {reply, error};
          _   -> Pid ! {reply, ok}
      end,
      loop(NewState);
    {request, Pid, {removeValue, {Id, Date, Type}}} ->
      NewState = pollution:removeValue(Id, Date, Type, State),
      case NewState of
         State -> Pid ! {reply, error};
         _     -> Pid ! {reply, ok}
      end,
      loop(NewState);
    {request, Pid, {getOneValue, {Id, Date, Type}}} ->
      Value = pollution:getOneValue(Id, Date, Type, State),
      case Value of
        {badKey, _} -> io:format("Couldn't find this key"), Pid ! {reply, error};
        _           -> Pid ! {reply, Value}
      end,
      loop(State);
    {request, Pid, {getStationMean, {Id, Type}}} ->
      Value = pollution:getStationMean(Id, Type, State),
      Pid ! {reply, Value},
      loop(State);
    {request, Pid, {getDailyMean, {Type, Day}}} ->
      Value = pollution:getDailyMean(Type, Day, State),
      Pid ! Value,
      loop(State);
    {request, Pid, {getHourlyStationData, {Id, Type}}} ->
      Value = pollution:getHourlyStationData(Id, Type, State),
      Pid ! Value,
      loop(State);
    {request, Pid, stop} ->
      io:format("Shutdown of pollution server~n"),
      Pid ! {reply, ok};
    {request, Pid, showMonitor} ->
      Pid ! {reply, State},
      loop(State);
    _ ->
      io:format("error~n"),
      loop(State)
  end.

%% client

call(Message) ->
  pollServer ! {request, self(), Message},
  receive
    {reply, error} -> io:format("error~n");
    {reply, Reply} -> Reply
  end.

addStation(Name, Coords) -> call({addStation, {Name, Coords}}).
addValue(Id, Date, Type, Value) -> call({addValue, {Id, Date, Type, Value}}).
removeValue(Id, Date, Type, Value) -> call({removeValue, {Id, Date, Type, Value}}).
getOneValue(Id, Date, Type) -> call({getOneValue, {Id, Date, Type}}).
getStationMean(Id, Type) -> call({getStationMean, {Id, Type}}).
getDailyMean(Type, Day) -> call({getDailyMean, {Type, Day}}).
getHourlyStationData(Id, Type) -> call({getHourlyStationData, {Id, Type}}).
stop() -> call(stop).

showMonitor() -> call(showMonitor).