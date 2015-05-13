-module(solar).
-behaviour(application).
-export([start/0, stop/0]).

start() ->
	client:start(),
	solar_system:start_link().

stop() ->
	solar_system:stop(),
	arbitrator:die().
