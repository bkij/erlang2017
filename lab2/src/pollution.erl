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

%% Data types
-record(measurement, {
  type :: PM10 | PM2_5 | TEMPERATURE,
  datetime :: calendar:datetime(),
  value
}).

-record(station, {
  name :: string(),
  location :: tuple(float, float),
  measurements :: list(measurement)
}).

-record(monitor, {
  stations :: list(station)
}
).

%% Functions
createMonitor() -> #monitor{}.

station_exists(Monitor, Station_name, Location) ->
  lists:any(fun(Station) -> Station#station.name == Station_name or Station#station.location == Location end, Monitor#monitor.stations).

getStation(Monitor, StationID) when is_tuple(StationID) ->
  lists:filter(fun(Station) -> Station#station.location == StationID end, Monitor#monitor.stations);
getStation(Monitor, StationID) ->
  lists:filter(fun(Station) -> Station#station.name == StationID end, Monitor#monitor.stations).

typeEqual(Measurement, Type) -> Measurement#measurement.type == Type.

dateEqual(Measurement, Date) -> Measurement#measurement.datetime == Date.

addStation(Monitor, Station_name, Location) when is_record(Monitor, monitor), is_tuple(Location) ->
  case station_exists(Monitor, Station_name, Location) of
    true -> #monitor{
      stations = [Monitor#monitor.stations | #station{name=Station_name, location=Location, measurements=[]}]
    };
    false -> Monitor
  end;
addStation(Monitor, Station_name, Location) -> erlang:error("Bad types in addStation").

addValue(Monitor, StationID, Datetime, Type, Value) ->
  case getStation(Monitor, StationID) of
    [Station] -> #monitor {
      stations = [lists:delete(Station, Monitor#monitor.stations) | #station {
        name = Station#station.name,
        location = Station#station.location,
        measurements = [ lists:filter(fun(Elem) ->  typeEqual(Elem, Type) end, Station#station.measurements) |
                         #measurement{type = Type, datetime = Datetime, value = Value}
                       ]
      }]
    };
    true -> Monitor
  end.

removeValue(Monitor, StationID, Datetime, Type) ->
  case getStation(Monitor, StationID) of
    [Station] -> #monitor {
      stations = [lists:delete(Station, Monitor#monitor.stations) | #station {
        name = Station#station.name,
        location = Station#station.location,
        measurements = lists:filter(fun(Elem) -> typeEqual(Elem, Type) and dateEqual(Elem, Datetime) end, Station#station.measurements)
      }]
    };
    true -> Monitor
  end.

getOneValue(Monitor, StationID, Datetime, Type) ->
  case getStation(Monitor, StationID) of
    [Station] -> lists:last(lists:filter(fun(Elem) -> typeEqual(Elem, Type) and dateEqual(Elem, Datetime) end, Station#station.measurements);
    true -> nil
  end.

typeFilter(Station, Type) -> lists:filter(fun(Elem) -> typeEqual(Elem, Type) end, Station#station.measurements).
measurementMap(MList) -> lists:map(fun(Elem) -> Elem#measurement.value end, MList).

getStationMean(Monitor, StationID, Type) ->
  case getStation(Monitor, StationID) of
    [Station] -> lists:sum(measurementMap(typeFilter(Station, Type))) /
                 length(typeFilter(Station, Type));
    true -> nil
  end.

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3]).
