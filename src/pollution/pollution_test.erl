%%%-------------------------------------------------------------------
%%% @author tkarkocha
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. kwi 2018 13:45
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("tkarkocha").

-include_lib("eunit/include/eunit.hrl").

-define(PM10, "PM10").
-define(PM25, "PM2,5").

-compile(export_all).

pollution_monitorCreate_test() ->
  ?assert(pollution:createMonitor() =:= #{}).

pollution_addStation_test() ->
  ?assert(#{{"abc", {12.3,3.21}} => #{}} =:= pollution:addStation("abc", {12.3,3.21}, #{})).

polluton_addStation2_test() ->
  M = #{{"abc", {1.2,3.4}} => #{}},
  ?assert(
    M =:= pollution:addStation("abc", {1.2,3.4}, M)
  ).

pollution_addStation3_test() ->
  M = #{{"abc", {1.2,3.4}} => #{}},
  M1 = M#{{"def", {9, 10}} => #{}},
  ?assert(
    M1 =:= pollution:addStation("def", {9, 10}, M)
  ).

pollution_addValue_test_() ->
  D = {{2018, 05, 28}, {12, 20, 22}},
  Dx = {2018, 05, 28, 12},
  M = #{{"a", {1, 3}} => #{}, {"b", {6, 9}} => #{}},
  M1 = M#{{"a", {1, 3}} => #{{Dx, ?PM10} => 13}},
  [
    ?_assert(M1 =:= pollution:addValue("a", D, ?PM10, 13, M)),
    ?_assert(M1 =:= pollution:addValue({1, 3}, D, ?PM10, 13, M)),
    ?_assertError(noSuchId, M =:= pollution:addValue("kak", D, ?PM10, 13, M))
  ].

pollution_removeValue_test_() ->
  D = {{2018, 05, 28}, {12, 20, 22}},
  Dx = {2018, 05, 28, 12},
  M = #{{"a", {1, 3}} => #{}, {"b", {6, 9}} => #{}},
  M1 = M#{{"a", {1, 3}} => #{{Dx, ?PM10} => 13}},
  [
    ?_assert(M =:= pollution:removeValue("a", D, ?PM10, M1)),
    ?_assert(M =:= pollution:removeValue({1, 3}, D, ?PM10, M1)),
    ?_assert(M1 =:= pollution:removeValue({1, 3}, D, ?PM25, M1))
  ].

pollution_getOneVal_test_() ->
  D = {{2018, 05, 28}, {12, 20, 22}},
  Dx = {2018, 05, 28, 12},
  M = #{{"a", {1, 3}} => #{}, {"b", {6, 9}} => #{}},
  M1 = M#{{"a", {1, 3}} => #{{Dx, ?PM10} => 13}, {"b", {6, 9}} => #{{Dx, ?PM25} => 113}},
  [
    ?_assert(113 =:= pollution:getOneValue("b", D, ?PM25, M1))
  ].

pollution_getDailyMean_test_() ->
  D = {{2018, 05, 28}, {12, 20, 22}},
  Dx = {2018, 05, 28, 12},
  Dy = {2018, 05, 29, 12},
  Dz = {2018, 05, 29, 13},
  Dxx = {2018, 05, 28, 13},
  M = #{
    {"a", {1, 3}} => #{
      {Dx, ?PM10} => 25,
      {Dz, ?PM25} => 100
    },
    {"b", {12, 54}} => #{
      {Dx, ?PM10} => 12,
      {Dy, ?PM10} => 44,
      {Dy, ?PM25} => 12,
      {Dxx, ?PM10} => 99
    },
    {"c", {23, 11}} => #{
      {Dx, ?PM10} => 88,
      {Dx, ?PM25} => 12,
      {Dy, ?PM10} => 15
    }
  },
  Val = (25+12+99+88) / 4,
  [
    ?_assert(Val =:= pollution:getDailyMean(?PM10, {2018, 05, 28}, M))
  ].

pollution_getHourlyStationData_test_() ->
  D1 = {2018, 05, 28, 12},
  D2 = {2018, 05, 28, 13},
  D3 = {2018, 05, 28, 14},
  D4 = {2018, 05, 28, 15},
  D5 = {2018, 05, 28, 16},
  D6 = {2018, 05, 29, 13},
  D7 = {2018, 05, 29, 14},
  D8 = {2018, 05, 29, 15},
  M = #{
    {"a", {1, 3}} => #{
      {D1, "PM10"} => 12,
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
      {D7, "PM10"} => 92,
      {D8, "PM10"} => 152
    },
    {"b", {34, 12}} => #{
      {D1, "PM10"} => 52,
      {D2, "PM25"} => 92,
      {D3, "PM10"} => 54
    }
  },
  Val = [{12, 12}, {13, (23+44)/2}, {14, (122+92)/2}, {15, (34+152)/2}, {16, 64}],
  [
    ?_assert(Val == pollution:getHourlyStationData("a", ?PM10, M))
  ].


