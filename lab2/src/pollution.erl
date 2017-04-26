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
createMonitor() -> #monitor{}

addStation(monitor, station_name, location)
  when is_record(monitor, monitor), is_tuple(location) -> #monitor{
    stations = [monitor.stations | #station{name=station_name, location=location, measurements=[]}]
};
addStation(monitor, station_name, location) -> erlang:error("Bad types in addStation").



%% API
-export([createMonitor/0, addStation/3]).
