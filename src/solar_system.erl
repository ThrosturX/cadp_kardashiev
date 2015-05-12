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

get_resource(Resource, iron) ->
	Resource#resources.iron;
get_resource(Resource, food) ->
	Resource#resources.food;
get_resource(Resource, gas) ->
	Resource#resources.gas.

%% Send to all nodes trade request
trade_request(TWant, THave) ->
	Fun = fun(N) -> send(rtrade, {TWant, THave}, N) end,
	lists:foreach(Fun, nodes()).	

%% Send to all nodes cancel request	
cancel_request(TWant, THave) ->
	Fun = fun(N) -> send(ctrade, {TWant, THave}, N) end,
	lists:foreach(Fun, nodes()).	

%% Check if offer is possible then send offer to Node
%%offer(Node, TWant, QT, THave, QH) ->
%%	io:format("Offer~n"),
%%	Reply = gen_server:call(solar_system, {trade_available, THave, QH}),
%%	if 
%%		Reply == noship ->
%%			ok;% GUI
%%		Reply == nores ->
%%			ok;% GUI
%%		true -> 
%%			R = sendWait(offer, {TWant, QT, THave, QH}, Node, 30000),
%%			if 
%%				R == {accept, Node} ->
%%					gen_server:cast(solar_system, {offer_accept, TWant, QT, THave, QH});
%%				true ->
%%					gen_server:cast(solar_system, {offer_reset, THave, QH})
%%			end
%%	end.	
spawner() -> 
	io:format("Spawner~n").

%%% Network functions 

connect(Node) ->
	net_kernel:connect_node(Node).

display_nodes() ->
	nodes().	

send(Type, Msg, Node) ->
	gen_server:cast({solar_system, Node}, {node(), Type, Msg}).

sendWait(Type, Msg, Node, Time) ->	
	gen_server:call({solar_system, Node}, {node(), Type, Msg}, Time).

	
%%% gen_server callbacks

init([]) -> 
	%<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	random:seed(now()),
	%The state consists of 3 dictionaries: Resources, Ships and TradeRes.
	Resources = dict:from_list([{iron, 0}, {food, 0}, {gas, 0}]),
	Ships = dict:from_list([{cargo_ship, 3}, {harvester, 3}, {escort, 3}]),
	TradeRes = dict:from_list([{iron, 0}, {food, 0}, {gas, 0}]),
	{ok, {Resources, Ships, TradeRes}}.
	
handle_call(resources, _From, State) ->
	{Resources, Ships, TradeRes} = State,
	io:format("Resources: ~p~n", [dict:to_list(Resources)]),
	io:format("Ships: ~p~n", [dict:to_list(Ships)]),
	io:format("TradeRes: ~p~n", [dict:to_list(TradeRes)]),
	{reply, [], State};
handle_call(start_harvest, _From, State) ->
	io:format("check if enough ships~n"),
	{Res, Ships, Trade} = State,
	H = dict:fetch(harvester, Ships),
	if 
		H == 0 -> 
			{reply, [noship], State};
		true -> 
			NewShips = dict:update_counter(harvester, -1, Ships),
			{reply, [ship], {Res, NewShips, Trade}}
	end;
handle_call({trade_available, THave, QH}, _From, State) ->
	io:format("Check if enough resources~n"),
	{Res, Ships, Trade} = State,
	C = Ships#ships.cargo_ship,
	if 
		C == 0 -> 
			{reply, [noship], State};
		true -> 
			R = get_resource(Res, THave),
			if 
				R >= QH -> 
					T = get_resource(Trade, THave),
					if 
						THave == iron ->
							{reply, [ok], 
							{Res#resources{iron = R - QH}, 
							Ships#ships{cargo_ship = C - 1}, 
							Trade#resources{iron = T + QH}}};
						THave == food ->
							{reply, [ok], 
							{Res#resources{food = R - QH}, 
							Ships#ships{cargo_ship = C - 1}, 
							Trade#resources{food = T + QH}}};
						true -> 
							{reply, [ok], 
							{Res#resources{gas = R - QH}, 
							Ships#ships{cargo_ship = C - 1}, 
							Trade#resources{gas = T + QH}}}
					end;
					true -> 
						{reply, [nores], State}
			end
	end;	
handle_call(_Msg, _From, State) ->
	{reply, [], State}.

handle_cast({harvest, Iron, Food, Gas}, State) ->
	io:format("harvest cast~n"),
	{Resources, Ships, Trade} = State,
	NewShips = dict:update_counter(harvester, 1, Ships),
	T1 = dict:update_counter(iron, Iron, Resources),
	T2 = dict:update_counter(food, Food, T1),
	NewRes = dict:update_counter(gas, Gas, T2),
	io:format("Iron: ~w~n", [Iron]),
	io:format("Food: ~w~n", [Food]),
	io:format("Gas: ~w~n", [Gas]),
	{noreply, {NewRes, NewShips, Trade}};
handle_cast({Node, msg, Msg}, State) ->
	io:format("Message from ~w: ~w~n", [Node, Msg]),
	{noreply, State};
handle_cast({Node, rtrade, {TWant, THave}}, State) ->
	io:format("Trade request from ~w: ~w, ~w~n", [Node, TWant, THave]),
	%TODO: Add request to list of trade requests in GUI
	{noreply, State};
handle_cast({Node, ctrade, {TWant, THave}}, State) ->
	io:format("Cancel request from ~w: ~w, ~w~n", [Node, TWant, THave]),
	%TODO: remove request to list of trade requests in GUI
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

