%%%-------------------------------------------------------------------
%%% @author kveld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Apr 2017 00:11
%%%-------------------------------------------------------------------
-module(pollution).
-author("kveld").
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getMaximumVariationStation/2]).

%% Data types
-record(measurement, {
  type,
  datetime :: calendar:datetime(),
  value
}).

-record(station, {
  name,
  location,
  measurements :: list(measurement)
}).

-record(monitor, {
  stations :: list(station)
}
).

%% Functions
createMonitor() -> #monitor{stations=[]}.

station_exists(Monitor, Station_name, Location) ->
  lists:any(fun(Station) -> Station#station.name == Station_name orelse Station#station.location == Location end, Monitor#monitor.stations).

getStation(Monitor, StationID) when is_tuple(StationID) ->
  lists:filter(fun(Station) -> Station#station.location == StationID end, Monitor#monitor.stations);
getStation(Monitor, StationID) ->
  lists:filter(fun(Station) -> Station#station.name == StationID end, Monitor#monitor.stations).

typeEqual(Measurement, Type) -> Measurement#measurement.type == Type.

dateEqual(Measurement, Date) -> Measurement#measurement.datetime == Date.

addStation(Monitor, Station_name, Location) when is_record(Monitor, monitor), is_tuple(Location) ->
  case station_exists(Monitor, Station_name, Location) of
    false -> #monitor{
      stations = [Monitor#monitor.stations ++ #station{name=Station_name, location=Location, measurements=[]}]
    };
    true -> Monitor
  end;
addStation(Monitor, Station_name, Location) -> erlang:error("Bad types in addStation").

addValue(Monitor, StationID, Datetime, Type, Value) ->
  case getStation(Monitor, StationID) of
    [Station] -> #monitor {
      stations = lists:delete(Station, Monitor#monitor.stations) ++ [#station {
        name = Station#station.name,
        location = Station#station.location,
        measurements = case getOneValue(Monitor, StationID, Datetime, Type) of
                         {} -> Station#station.measurements ++ [#measurement{type = Type, datetime = Datetime, value = Value}];
                         _ -> Station#station.measurements
                       end

      }]
    };
    _ -> Monitor
  end.

removeValue(Monitor, StationID, Datetime, Type) ->
  case getStation(Monitor, StationID) of
    [Station] -> #monitor {
      stations = [lists:delete(Station, Monitor#monitor.stations) ++ #station {
        name = Station#station.name,
        location = Station#station.location,
        measurements = lists:filter(fun(Elem) -> typeEqual(Elem, Type) and dateEqual(Elem, Datetime) end, Station#station.measurements)
      }]
    };
    _ -> Monitor
  end.

getOneValue(Monitor, StationID, Datetime, Type) ->
  case getStation(Monitor, StationID) of
  [Station] -> emptyOrSome(lists:filter(fun(Elem) -> typeEqual(Elem, Type) and dateEqual(Elem, Datetime) end, Station#station.measurements));
    _ -> {}
  end.

typeFilter(Station, Type) -> lists:filter(fun(Elem) -> typeEqual(Elem, Type) end, Station#station.measurements).
measurementMap(MList) -> lists:map(fun(Elem) -> Elem#measurement.value end, MList).

getStationMean(Monitor, StationID, Type) ->
  case getStation(Monitor, StationID) of
    [Station] -> lists:sum(measurementMap(typeFilter(Station, Type))) /
                 erlang:length(typeFilter(Station, Type));
    true -> nil
  end.

dateTypeFilter(MList, Date, Type) -> lists:filter(fun(Elem) -> Elem#measurement.datetime == Date andalso
                                                               Elem#measurement.type == Type
                                                  end, MList).

dailyFilter(MList, Date, Type) -> lists:filter(fun(Elem) -> getDateFromDatetime(Elem#measurement.datetime) == getDateFromDatetime(Date) andalso
  Elem#measurement.type == Type
                                               end, MList).

getAllMeasurements(Monitor) -> lists:foldl(fun(Station, Res) -> Station#station.measurements ++ Res end,
                                           [],
                                           Monitor#monitor.stations).

getDailyMean(Monitor, Datetime, Type) ->
  lists:sum(measurementMap(dailyFilter(getAllMeasurements(Monitor), Datetime, Type))) /
  erlang:length(dailyFilter(getAllMeasurements(Monitor), Datetime, Type)).

varMax(List) ->
  lists:foldl(fun({Name, Value}, {MaxName, Max}) ->
    case Value >= Max of
      true -> {Name, Value};
      false -> {MaxName, Max}
    end
              end,
    {"", 0},
    List).

getMaximumVariationStation(Monitor, Type) ->
  varMax(variationMap(Monitor#monitor.stations, Type)).

variationMap(Stations, Type) ->
  TypeStations = lists:map(fun(Station) -> {Station#station.name, typeFilter(Station, Type)} end, Stations),
  lists:map(fun({Name, Measurements}) -> {
    Name, lists:max(lists:map(fun(El) -> El#measurement.value end, Measurements)) - lists:min(lists:map(fun(El) -> El#measurement.value end, Measurements))
  } end, TypeStations).


getDateFromDatetime({X, _}) -> X.

emptyOrSome([X]) -> X;
emptyOrSome([]) -> {}.