%%%-------------------------------------------------------------------
%%% @author kveld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. May 2017 14:29
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("kveld").

-include_lib("eunit/include/eunit.hrl").


monitor_creation_test() ->
  ?assertMatch({monitor, []}, pollution:createMonitor()).

pollution_suite_test_() ->
  {
    foreach,
    fun setup_monitor/0,
    [
      fun two_stations_same_name/1,
      fun two_stations_same_coords/1,
      fun nonexistent_station_value/1,
      fun identical_measurements/1,
      fun value_retrieval/1,
      fun station_mean/1,
      fun daily_mean/1,
      fun var_max/1
    ]
  }.

setup_monitor() ->
  M = pollution:createMonitor(),
  M1 = pollution:addStation(M, "A", {30.03, 50.03}),
  pollution:addValue(M1, "A", {{11,11,11},{11,11,11}}, "PM10", 59).

two_stations_same_name(Monitor) ->
  [?_assertEqual(Monitor, pollution:addStation(Monitor, "A", {0,0}))].

two_stations_same_coords(Monitor) ->
  [?_assertEqual(Monitor, pollution:addStation(Monitor, "B", {30.03, 50.03}))].

nonexistent_station_value(Monitor) ->
  [?_assertEqual(Monitor, pollution:addValue(Monitor, "C", calendar:local_time(), "PM2.5", 113))].

identical_measurements(Monitor) ->
  Now = calendar:local_time(),
  M1 = pollution:addValue(Monitor, "A", Now, "PM10", 60),
  M2 = pollution:addValue(M1, "A", Now, "PM10", 60),
  [?_assertEqual(M1, M2)].

value_retrieval(Monitor) ->
  [?_assertMatch({measurement, "PM10", {{11,11,11},{11,11,11}}, 59}, pollution:getOneValue(Monitor, "A", {{11,11,11},{11,11,11}}, "PM10"))].

station_mean(Monitor) ->
  [?_assertEqual(59.0, pollution:getStationMean(Monitor, {30.03, 50.03}, "PM10"))].

daily_mean(Monitor) ->
  [?_assertEqual(59.0, pollution:getDailyMean(Monitor, {{11,11,11},{11,11,11}}, "PM10"))].

var_max(Monitor) ->
  [?assertMatch({"A", 0}, pollution:getMaximumVariationStation(Monitor, "PM10"))].