-module(solar_system).
-behavior(gen_server).
	
-export([start_link/0,
		home_planet/0,
		spawner/0,
		print_resources/0,
		harvest/1,
		harvesting/1,
		stop/0,
		connect/1,
		display_nodes/0,
		send/3,
		trade_request/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(SERVER, ?MODULE).
-define(MAX_HARVEST, 10).
-define(MAX_HARVEST_TIME, 12000).
-define(MIN_HARVEST_TIME, 6000).


-record(resources, {iron = 0, food = 0, gas = 0}).
-record(ships, {cargo_ship = 3, harvester = 3, escort = 3}).

random(N,M) -> 
	N + random:uniform(M-N).

sleep(T) ->
	receive
	after T -> true
	end.

randomSleep(T) ->
	sleep(random:uniform(T)).
randomSleep(N,M) ->
	sleep(random(N,M)).

start_link() ->
	register(home, spawn(solar_system, home_planet, [])),
	spawn(solar_system, spawner, []),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).
	
home_planet() -> 
	io:format("Home planet~n").

print_resources() ->
	gen_server:call(solar_system, resources).

harvest(Type) ->
	io:format("Harvest~n"),
	Reply = gen_server:call(solar_system, start_harvest),
	io:format("reply: ~p~n", [Reply]),
	spawn(solar_system, harvesting, [Type]).
	
harvesting(gas) ->
	random:seed(now()),
	io:format("Harvesting~n"),
	randomSleep(?MIN_HARVEST_TIME, ?MAX_HARVEST_TIME),
	gen_server:cast(solar_system, {harvest, 0, 0, random:uniform(?MAX_HARVEST)});
harvesting(asteroid) ->
	random:seed(now()),
	io:format("Harvesting~n"),
	randomSleep(?MIN_HARVEST_TIME, ?MAX_HARVEST_TIME),
	gen_server:cast(solar_system, {harvest, random:uniform(?MAX_HARVEST), 0, 0});
harvesting(mclass) ->
	random:seed(now()),
	io:format("Harvesting~n"),
	randomSleep(?MIN_HARVEST_TIME, ?MAX_HARVEST_TIME),
	gen_server:cast(solar_system, {harvest, 0, random:uniform(?MAX_HARVEST), 0}).


trade_request(TWant, THave) ->
	Fun = fun(N) -> send(rtrade, {TWant, THave}, N) end,
	lists:foreach(Fun, nodes()).	
	

spawner() -> 
	io:format("Spawner~n").

%%% Network functions 

connect(Node) ->
	net_kernel:connect_node(Node).

display_nodes() ->
	nodes().	

send(Type, Msg, Node) ->
	gen_server:cast({solar_system, Node}, {node(), Type, Msg}).
	
	
%%% gen_server callbacks

init([]) -> 
	%<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	random:seed(now()),
	{ok, {#resources{}, #ships{}}}.
	
handle_call(resources, _From, State) ->
	{Resources, Ships} = State,
	io:format("Resources: ~p~n", [Resources]),
	io:format("Ships: ~p~n", [Ships]),
	{reply, [], State};
handle_call(start_harvest, _From, State) ->
	io:format("check if enough ships~n"),
	{Res, Ships} = State,
	H = Ships#ships.harvester,
	if H == 0 -> {reply, [noship], State};
	true -> {reply, [ship], {Res, Ships#ships{harvester = H - 1}}}
	end;	
handle_call(_Msg, _From, State) ->
	{reply, [], State}.

handle_cast({harvest, Iron, Food, Gas}, State) ->
	io:format("harvest cast~n"),
	{Resources, Ships} = State,
	H = Ships#ships.harvester,
	A = Resources#resources.iron,
	io:format("Iron: ~w~n", [Iron]),
	B = Resources#resources.food,
	io:format("Food: ~w~n", [Food]),
	C = Resources#resources.gas,
	io:format("Gas: ~w~n", [Gas]),
	{noreply, {#resources{iron = Iron+A, food = Food+B, gas = Gas+C}, Ships#ships{harvester = H + 1}}};	
handle_cast({Node, msg, Msg}, State) ->
	io:format("Message from ~w: ~w~n", [Node, Msg]),
	{noreply, State};
handle_cast({Node, rtrade, {TWant, THave}}, State) ->
	io:format("Trade request from ~w: ~w, ~w~n", [Node, TWant, THave]),
	%TODO: Add request to list of trade requests in GUI
	{noreply, State};
handle_cast(stop, State) ->
	io:format("Stopping solar_system ~n"),
	{stop, normal, State}.

handle_info(Info, State) ->
	io:format("~p~n", [Info]),
	{noreply, State}.

terminate(normal, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

