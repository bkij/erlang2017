%%%-------------------------------------------------------------------
%%% @author kveld
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Jun 2017 16:34
%%%-------------------------------------------------------------------
-module(r_pollution_server_supervisor).
-behaviour(supervisor).
-author("kveld").

%% API
-export([start_link/0, init/1]).


start_link() -> supervisor:start_link({local, pollutionSupervisor}, ?MODULE, []).

init(_) -> {
  ok,
  {
    {one_for_one, 5, 2000},
    [
      {r_pollution_server, {r_pollution_server, start_link, []}, permanent, 1000, worker, [r_pollution_server]}
    ]
  }
}.

