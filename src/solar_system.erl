-module(solar_system).
-export([start/0, home_planet/0]).

start() -> 
	register(home, spawn(solar_system, home_planet, [])).

home_planet() -> io:format("Home planet").

