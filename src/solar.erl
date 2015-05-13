-module(solar).
-export([start/0, stop/0]).

start() ->
	solar_system:set_node_name('leeroy@jenkins'),
	client:start(),
	solar_system:start_link().

stop() ->
	solar_system:stop(),
	arbitrator:die().
