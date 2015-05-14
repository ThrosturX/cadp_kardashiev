-module(solar).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
	client:start(),
	solar_system:start_link().

stop(_State) ->
	solar_system:stop(),
	arbitrator:die().
