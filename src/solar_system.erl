-module(solar_system).
-export([start/0, home_planet/0]).

-record(resource, {iron = 0, food = 0, livestock = 0}).

start() -> 
	register(home, spawn(solar_system, home_planet, [])).i
	

home_planet() -> io:format("Home planet").

