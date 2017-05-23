%%%-------------------------------------------------------------------
%%% @author kveld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. May 2017 19:53
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("kveld").

%% API
-export([start/0, stop/0]).

start() ->
  Pid = spawn(fun() -> pollutionServer() end),
  register(pollution_server, Pid).

stop() ->
  exit(whereis(pollution_server), ok).

pollutionServer() ->
  receive
    create -> pollution:createMonitor(), pollutionServer();
    {addStation, Monitor, StationName, Location} -> pollution:addStation(Monitor, StationName, Location), pollutionServer();
    {addValue, Monitor, StationID, Datetime, Type, Value} -> pollution:addValue(Monitor, StationID, Datetime, Type, Value), pollutionServer();
    {removeValue, Monitor, StationID, Datetime, Type} -> pollution:removeValue(Monitor, StationID, Datetime, Type), pollutionServer();
    {getOneValue, Monitor, StationID, Datetime, Type} -> pollution:getOneValue(Monitor, StationID, Datetime, Type), pollutionServer();
    {getStationMean, Monitor, StationID, Type} -> pollution:getStationMean(Monitor, StationID, Type), pollutionServer();
    {getDailyMean, Monitor, Datetime, Type} -> pollution:getDailyMean(Monitor, Datetime, Type), pollutionServer();
    {getMaximumVariationStation, Monitor, Type} -> pollution:getMaximumVariationStation(Monitor, Type), pollutionServer()
  end.
