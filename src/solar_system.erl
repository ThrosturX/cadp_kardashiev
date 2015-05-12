-module(solar_system).
-behavior(gen_server).

-export([start/0, home_planet/0]).

-define(SERVER, ?MODULE).

-record(resource, {iron = 0, food = 0, livestock = 0}).

start() ->
	register(solar, self()),
	register(home, spawn(solar_system, home_planet, [])),
	spawn(solar_system, spawner, []),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

home_planet() -> io:format("Home planet").

