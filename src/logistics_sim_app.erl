%%%-------------------------------------------------------------------
%% @doc logistics_sim public API
%% @end
%%%-------------------------------------------------------------------

-module(logistics_sim_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    logistics_sim_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
