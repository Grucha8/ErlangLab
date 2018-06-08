%%%-------------------------------------------------------------------
%%% @author tkarkocha
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. maj 2018 12:45
%%%-------------------------------------------------------------------
-module(pollution_superviser).
-author("tkarkocha").

%% API
-export([start_link/0, init/1]).

-record(jd, {name, myLovelyMap}).

start_link() ->
  Table = ets:new(globalTable, [public, named_table, {keypos, #jd.name}]),
  ets:insert(globalTable, #jd{name=one, myLovelyMap=pollution:createMonitor()}),
  supervisor:start_link({local, pollutionSupervisor},
    ?MODULE, ets:lookup(globalTable, one)#jd.myLovelyMap).

init(InitValue) ->
  {ok, {
    {one_for_all, 2, 3},
    [ {pollutionServer,
      {pollution_gen_server, start, [InitValue]},
      permanent, brutal_kill, worker, [pollution_gen_server]}
    ]}
  }.