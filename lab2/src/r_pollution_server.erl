%%%-------------------------------------------------------------------
%%% @author kveld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. May 2017 11:41
%%%-------------------------------------------------------------------
-module(r_pollution_server).
-author("kveld").
-behavior(gen_server).

%% API
-export([start_link/0, create/1, addStation/4, addValue/6, removeValue/5, getOneValue/5, getStationMean/4, getDailyMean/4, getMaximumVariationStation/3, crash/1]).
-export([init/1, handle_call/2, handle_call/3, terminate/2]).

%% START
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init(_) -> {ok, stateless}.

%% CLIENT API
create(Pid) -> gen_server:call(Pid, create).
addStation(Pid, Monitor, StationName, Location) -> gen_server:call(Pid, {addStation, Monitor, StationName, Location}).
addValue(Pid, Monitor, StationID, Datetime, Type, Value) -> gen_server:call(Pid, {addValue, Monitor, StationID, Datetime, Type, Value}).
removeValue(Pid, Monitor, StationID, Datetime, Type) -> gen_server:call(Pid, {removeValue, Monitor, StationID, Datetime, Type}).
getOneValue(Pid, Monitor, StationID, Datetime, Type) -> gen_server:call(Pid, {getOneValue, Monitor, StationID, Datetime, Type}).
getStationMean(Pid, Monitor, StationID, Type) -> gen_server:call(Pid, {getStationMean, Monitor, StationID, Type}).
getDailyMean(Pid, Monitor, Datetime, Type) -> gen_server:call(Pid, {getDailyMean, Monitor, Datetime, Type}).
getMaximumVariationStation(Pid, Monitor, Type) -> gen_server:call(Pid, {getMaxVar, Monitor, Type}).

%% SERVER API
handle_call(X, _From) ->
  case X of
    create -> {reply, {create_reply, pollution:createMonitor()}, stateless};
    {addStation, Monitor, StationName, Location} -> {reply, {addS_reply, pollution:addStation(Monitor, StationName, Location)}, stateless};
    {addValue, Monitor, StationID, Datetime ,Type, Value} -> {reply, {addV_reply, pollution:addValue(Monitor, StationID, Datetime, Type, Value)}, stateless};
    {removeValue, Monitor, StationID, Datetime, Type} -> {reply, {rmv_reply, pollution:removeValue(Monitor, StationID, Datetime, Type)}, stateless};
    {getOneValue, Monitor, StationID, Datetime, Type} -> {reply, {get_reply, pollution:getOneValue(Monitor, StationID, Datetime, Type)}, stateless};
    {getStationMean, Monitor, StationID, Type} -> {reply, {station_mean_reply, pollution:getStationMean(Monitor, StationID, Type)}, stateless};
    {getDailyMean, Monitor, Datetime, Type} -> {reply, {daily_mean_reply, pollution:getDailyMean(Monitor, Datetime, Type)}, stateless};
    {getMaxVar, Monitor, Type} -> {reply, {max_var_reply, pollution:getMaximumVariationStation(Monitor, Type)}, stateless};
    _ -> {stop, "terminated", stateless}
  end.

crash(Pid) -> gen_server:call(Pid, {haha}).

handle_call(X, _From, _) -> handle_call(X, _From).

terminate(Status, _) -> io:format("TERMINATED").