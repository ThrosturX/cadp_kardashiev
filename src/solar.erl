-module(solar).
-export([start/0, stop/0]).

start() ->
	erlang:set_cookie(node(), kaka),
	client:start(),
	solar_system:start_link().

stop() ->
	solar_system:stop(),
	arbitrator:die().
