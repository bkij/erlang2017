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
  datetime :: calendar:datetime()
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

station_exists(Monitor, Station_name) -> lists:any(fun(Station) -> Station#station.name == Station_name end, Monitor#monitor.stations).

addStation(Monitor, Station_name, Location) when is_record(Monitor, monitor), is_tuple(Location) ->
  case station_exists(Monitor, Station_name) of
    true -> #monitor{
      stations = [Monitor#monitor.stations | #station{name=Station_name, location=Location, measurements=[]}]
    };
    false -> Monitor
  end;
addStation(Monitor, Station_name, Location) -> erlang:error("Bad types in addStation").


%% API
-export([createMonitor/0, addStation/3]).
