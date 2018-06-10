%%%-------------------------------------------------------------------
%%% @author tkarkocha
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. kwi 2018 13:45
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("tkarkocha").

-include_lib("eunit/include/eunit.hrl").

-define(PM10, "PM10").
-define(PM25, "PM2,5").

-compile(export_all).

pollution_monitorCreate_test() ->
  pollution_server:start(),
  ?assert(#{} =:= pollution_server:showMonitor()).

polluton_addStation2_test() ->
  M = #{{"a", {1,3}} => #{}},
  pollution_server:addStation("a", {1,3}),
  ?assert(
    M =:= pollution_server:showMonitor()
  ).

pollution_addStation3_test() ->
  M1 = #{{"a", {1, 3}} => #{}, {"b", {6, 9}} => #{}},
  pollution_server:addStation("b", {6, 9}),
  ?assert(
    M1 =:= pollution_server:showMonitor()
  ).

pollution_addValue_test_() ->
  D = {{2018, 05, 28}, {12, 20, 22}},
  Dx = {2018, 05, 28, 12},
  M = #{{"a", {1, 3}} => #{}, {"b", {6, 9}} => #{}},
  M1 = M#{{"a", {1, 3}} => #{{Dx, ?PM10} => 13}},
  pollution_server:addValue("a", D, ?PM10, 13),
  [
    ?_assert(M1 =:= pollution_server:showMonitor())
    %?_assertError(noSuchId, M =:= pollution:addValue("kak", D, ?PM10, 13, M))
  ].

pollution_removeValue_test_() ->
  D = {{2018, 05, 28}, {12, 20, 22}},
  Dx = {2018, 05, 28, 12},
  M = #{{"a", {1, 3}} => #{}, {"b", {6, 9}} => #{}},
  M1 = M#{{"a", {1, 3}} => #{{Dx, ?PM10} => 13}},
  pollution_server:removeValue("a", D, ?PM10),
  [
    ?_assert(M =:= pollution_server:showMonitor())
  ].

pollution_getOneVal_test_() ->
  D = {{2018, 05, 28}, {12, 20, 22}},
  Dx = {2018, 05, 28, 12},
  M = #{{"a", {1, 3}} => #{}, {"b", {6, 9}} => #{}},
  M1 = M#{{"a", {1, 3}} => #{{Dx, ?PM10} => 25}, {"b", {6, 9}} => #{{Dx, ?PM25} => 12}},
  pollution_server:addValue("a", D, ?PM10, 25),
  pollution_server:addValue("b", D, ?PM25, 12),
  [
    ?_assert(12 =:= pollution_server:getOneValue("b", D, ?PM25))
  ].

pollution_getDailyMean_test_() ->
  D = {{2018, 05, 28}, {12, 20, 22}},
  Dx = {2018, 05, 28, 12},
  Dy = {{2018, 05, 29},{13,20,22}},
  Dz = {{2018, 05, 29},{14,20,20}},
  Dxx ={{2018, 05, 28},{13,23,22}},
%%  M = #{
%%    {"a", {1, 3}} => #{
%%      {Dx, ?PM10} => 25,  %
%%      {Dz, ?PM25} => 100
%%    },
%%    {"b", {12, 54}} => #{
%%      {Dx, ?PM25} => 12,  %
%%      {Dy, ?PM10} => 44,
%%      {Dy, ?PM25} => 12,
%%      {Dxx, ?PM10} => 99
%%    },
%%    {"c", {23, 11}} => #{
%%      {Dx, ?PM10} => 88,
%%      {Dx, ?PM25} => 12,
%%      {Dy, ?PM10} => 15
%%    }
%%  },
  pollution_server:addValue("a", Dz, ?PM25, 100),
  pollution_server:addValue("b", Dy, ?PM10, 44),
  pollution_server:addValue("b", Dy, ?PM25, 12),
  pollution_server:addValue("b", Dxx, ?PM10, 99),
  pollution_server:addStation("c", {23, 11}),
  pollution_server:addValue("c", D, ?PM10, 88),
  pollution_server:addValue("c", D, ?PM25, 12),
  pollution_server:addValue("c", Dy, ?PM10, 15),
  Val = (25+99+88) / 3,
  [
    ?_assert(Val =:= pollution_server:getDailyMean(?PM10, {2018, 05, 28}))
  ].

pollution_getHourlyStationData_test_() ->
  D1 = {{2018, 05, 28}, {12,20,20}},  %
  D2 = {{2018, 05, 28}, {13,20,20}},  %
  D3 = {{2018, 05, 28}, {14,20,20}},
  D4 = {{2018, 05, 28}, {15,20,20}},
  D5 = {{2018, 05, 28}, {16,20,20}},
  D6 = {{2018, 05, 29}, {13,20,20}},  %
  D7 = {{2018, 05, 29}, {14,20,20}},  %
  D8 = {{2018, 05, 29}, {15,20,20}},
  M = #{
    {"a", {1, 3}} => #{
      {D1, "PM10"} => 25, %
      {D1, "PM25"} => 54,
      {D2, "PM10"} => 23,
      {D2, "PM25"} => 90,
      {D3, "PM10"} => 122,
      {D3, "PM25"} => 10,
      {D4, "PM10"} => 34,
      {D4, "PM25"} => 76,
      {D5, "PM10"} => 64,
      {D5, "PM25"} => 34,
      {D6, "PM10"} => 44,
      {D7, "PM25"} => 100,%
      {D8, "PM10"} => 152
    },
    {"b", {12, 54}} => #{
      {D1, "PM25"} => 12,%
      {D2, "PM10"} => 99,%
      {D3, "PM10"} => 54
    }
  },
  pollution_server:addValue("a", D1, ?PM25, 54),
  pollution_server:addValue("a", D2, ?PM10, 23),
  pollution_server:addValue("a", D2, ?PM25, 90),
  pollution_server:addValue("a", D3, ?PM10, 122),
  pollution_server:addValue("a", D3, ?PM25, 10),
  pollution_server:addValue("a", D4, ?PM10, 34),
  pollution_server:addValue("a", D4, ?PM25, 76),
  pollution_server:addValue("a", D5, ?PM10, 64),
  pollution_server:addValue("a", D5, ?PM25, 34),
  pollution_server:addValue("a", D6, ?PM10, 44),
  pollution_server:addValue("a", D8, ?PM10, 152),
  pollution_server:addValue("b", D3, ?PM10, 54),

  Val = [{12, 25}, {13, (23+44)/2}, {14, 122}, {15, (34+152)/2}, {16, 64}],
  [
    ?_assert(Val == pollution_server:getHourlyStationData("a", ?PM10))
   % ?_assert(stop ==   pollution_server:stop())
  ].


