-module(solar_system).
-behavior(gen_server).

	
-export([start_link/0,
		home_planet/0,
		spawner/0,
		print_resources/0,
		harvest/0,
		harvesting/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(SERVER, ?MODULE).

-record(resources, {iron = 0, food = 0, livestock = 0}).

sleep(T) ->
	receive
	after T -> true
	end.

randomSleep(T) ->
	sleep(random:uniform(T)).

start_link() ->
	<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	random:seed(A,B,C),
	register(solar, self()),
	register(home, spawn(solar_system, home_planet, [])),
	spawn(solar_system, spawner, []),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

home_planet() -> 
	io:format("Home planet~n").

print_resources() ->
	gen_server:call(solar_system, resources).

harvest() ->
	io:format("Harvest~n"),
	spawn(solar_system, harvesting, []).
	
harvesting() ->
	io:format("Harvesting~n"),
	randomSleep(2000),
	gen_server:cast(solar_system, {harvest, 10, 0, 0}).	

spawner() -> 
	io:format("Spawner~n").

	
%%% gen_server callbacks

init([]) -> 	
	{ok, {#resources{}, 0}}.
	
handle_call(resources, _From, State) ->
	{Resources, _} = State,
	io:format("Resources: ~p~n", [Resources]),
	{reply, [], State};
handle_call(_Msg, _From, State) ->
	{reply, [], State}.

handle_cast({harvest, Iron, Food, Livestock}, State) ->
	io:format("harvest cast~n"),
	{Resources, Rest} = State,
	A = Resources#resources.iron,
	B = Resources#resources.food,
	C = Resources#resources.livestock,
	{noreply, {#resources{iron = Iron+A, food = Food+B, livestock = Livestock+C}, Rest}};	
handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info(Info, State) ->
	io:format("~p~n", [Info]),
	{noreply, State}.

terminate(normal, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

