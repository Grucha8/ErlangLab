%%%-------------------------------------------------------------------
%%% @author tkarkocha
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. maj 2018 11:57
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-behaviour(gen_server).

-author("tkarkocha").

%% API
-export([start/1, init/1, handle_cast/2, handle_call/3, terminate/2, handle_info/2]).
-export([stop/0, addStation/2, addValue/4, showMonitor/0, crash/0]).
-export([getStationMean/2, getDailyMean/2, getHourlyStationMean/2, getOneValue/3]).

-define(SERVER, ?MODULE).


start(InitValue) ->
  [{_, Monitor}] = ets:lookup(InitValue, monitor),
  gen_server:start_link(
    {local, ?SERVER},
    ?MODULE,
    Monitor, []).

init(State) ->
  {ok, State}.


%% user interface
stop() -> gen_server:cast(?SERVER, stop).
addStation(Name, Coords) -> gen_server:cast(?SERVER, {addS, Name, Coords}).
addValue(Id, Date, Type, Value) -> gen_server:cast(?SERVER, {addV, Id, Date, Type, Value}).
getOneValue(Id, Date, Type) -> gen_server:call(?SERVER, {getOne, Id, Date, Type}).
getStationMean(Id, Type) -> gen_server:call(?SERVER, {getSMean, Id, Type}).
getDailyMean(Type, Day) -> gen_server:call(?SERVER, {getDM, Type, Day}).
getHourlyStationMean(Id, Type) -> gen_server:call(?SERVER, {getHSD, Id, Type}).
showMonitor() -> gen_server:call(?SERVER, show).
crash() -> gen_server:cast(?SERVER, crash).


%% callbacks
handle_cast(stop, State) -> {stop, normal, State};

handle_cast(crash, _From) -> 1/0;

handle_cast({addS, Name, Coords}, State) ->
  NewState = pollution:addStation(Name, Coords, State),
  ets:insert(table, {monitor, NewState}),
  {noreply, NewState};

handle_cast({addV, Id, Date, Type, Value}, State) ->
  NewState = pollution:addValue(Id, Date, Type, Value, State),
  ets:insert(table, {monitor, NewState}),
  {noreply, NewState}.


handle_call(show, _From, State) -> {reply, State, State};
handle_call({getOne, Id, Date, Type}, _From, State) ->
  {reply, pollution:getOneValue(Id, Date, Type, State), State};
handle_call({getSMean, Id, Type}, _From, State) ->
  {reply, pollution:getStationMean(Id, Type, State)};
handle_call({getDM, Type, Day}, _From, State) ->
  {reply, pollution:getDailyMean(Type, Day, State)};
handle_call({getHSD, Id, Type}, _From, State) ->
  {reply, pollution:getHourlyStationData(Id, Type, State)}.


handle_info(_Message, State) ->
  io:format("Wrong message~n"),
  {noreply, State}.


terminate(Reason, State) ->
  io:format("Ending pollution server...~n"),
  State.
