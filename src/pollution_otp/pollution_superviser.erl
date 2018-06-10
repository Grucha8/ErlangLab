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


start_link() ->
  ets:new(table, [set, named_table, public]),
  ets:insert(table, {monitor, pollution:createMonitor()}),
  supervisor:start_link({local, pollutionSupervisor},
    ?MODULE, table).

init(InitValue) ->
  {ok, {
    {one_for_all, 2, 3},
    [ {pollutionServer,
      {pollution_gen_server, start, [InitValue]},
      permanent, brutal_kill, worker, [pollution_gen_server]}
    ]}
  }.