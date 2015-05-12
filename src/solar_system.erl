-module(solar_system).
-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).
-export([start_link/0, home_planet/0, spawner/0]).

-define(SERVER, ?MODULE).

-record(resource, {iron = 0, food = 0, livestock = 0}).

start_link() ->
	register(solar, self()),
	register(home, spawn(solar_system, home_planet, [])),
	spawn(solar_system, spawner, []),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) -> 
	
	{ok, []}.

home_planet() -> io:format("Home planet~n").

spawner() -> io:format("Spawner~n").

handle_call(_Msg, _From, State) ->
	{reply, [], State}.

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info(Info, State) ->
	io:format("~p~n", Info),
	{noreply, State}.

terminate(normal, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

