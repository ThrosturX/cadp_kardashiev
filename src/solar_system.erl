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
		trade_request/2,
		offer/5,
		build/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(SERVER, ?MODULE).
-define(MAX_HARVEST, 10).
-define(MAX_HARVEST_TIME, 4000).
-define(MIN_HARVEST_TIME, 2000).
-define(MAX_BUILD_TIME, 10000).
-define(MIN_BUILD_TIME, 4000).

random(N) ->
	<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	random:seed(A,B,C),
	random:uniform(N).

random(N,M) -> 
	<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	random:seed(A,B,C),
	N + random:uniform(M-N).

sleep(T) ->
	receive
	after T -> true
	end.

randomSleep(T) ->
	sleep(random(T)).
randomSleep(N,M) ->
	sleep(random(N,M)).

% returns the resources formatted for the arbitrator
list_resources(State) ->
	{Res, _, _} = State,
	dict:to_list(Res).

list_ships(State) -> 
	{_, Ships, _} = State,
	dict:to_list(Ships).
	
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

%% Build function checks the Type of ship and 
%% if there are enough resources to build the ship 
build(Type) ->
	io:format("Build: ~w~n", [Type]),
	if
		Type == 'Death Ray' ->
			Reply = gen_server:call(solar_system, {build, 1000, 1000, 1000}),
			if
				Reply == build_ok ->
					io:format("Building: ~w~n", [Type]),
					gen_server:cast(solar_system, {building, Type});
				true ->
					io:format("Not enough resources~n")
			end;
		Type == 'Harvester' ->
			Reply = gen_server:call(solar_system, {build, 10, 10, 10}),
			if
				Reply == build_ok ->
					io:format("Building: ~w~n", [Type]),
					gen_server:cast(solar_system, {building, Type});
				true ->
					io:format("Not enough resources~n")
			end;
		Type == 'Cargo Ship' ->
			Reply = gen_server:call(solar_system, {build, 30, 30, 30}),
			if
				Reply == build_ok ->
					io:format("Building: ~w~n", [Type]),
					gen_server:cast(solar_system, {building, Type});
				true ->
					io:format("Not enough resources~n")
			end;
		Type == 'Escort' ->
			Reply = gen_server:call(solar_system, {build, 60, 60, 60}),
			if
				Reply == build_ok ->
					io:format("Building: ~w~n", [Type]),
					gen_server:cast(solar_system, {building, Type});
				true ->
					io:format("Not enough resources~n")
			end;
		true ->
			io:format("Unkown Type: ~w~n", [Type]),
			false
	end.

% Start a harvesting operation on a location of type 'Type'
% If no harvesters are available, nothing happens
harvest(Type) ->
	io:format("Harvest~n"),
	Reply = gen_server:call(solar_system, start_harvest),
	io:format("reply: ~p~n", [Reply]),
	if
		Reply == ship ->
			spawn(solar_system, harvesting, [Type]);
		true ->
			false
	end.

% Perform a harvesting operation of the given type and after waiting for  
% some time, sends the result to the server
harvesting(Type) ->
	io:format("Harvesting~n"),
	randomSleep(?MIN_HARVEST_TIME, ?MAX_HARVEST_TIME),
	gen_server:cast(solar_system, {harvest, Type, random:uniform(?MAX_HARVEST)}).

%% Send to all nodes trade request
trade_request(TWant, THave) ->
	Fun = fun(N) -> send(rtrade, {TWant, THave}, N) end,
	lists:foreach(Fun, nodes()).	

%% Send to all nodes cancel request	
cancel_request(TWant, THave) ->
	Fun = fun(N) -> send(ctrade, {TWant, THave}, N) end,
	lists:foreach(Fun, nodes()).	

%% Check if offer is possible then send offer to Node
offer(Node, TWant, QT, THave, QH) ->
	io:format("Offer~n"),
	Reply = gen_server:call(solar_system, {reserve_resource, THave, QH}),
	if 
		Reply == noship ->
			{ok, Reply};% TODO: Inform GUI
		Reply == nores ->
			{ok, Reply};% TODO: Inform GUI
		true ->
			% TODO: Update offers in GUI.
			send(offer, {TWant, QT, THave, QH}, Node)
	end.
	
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
	%The state consists of 3 dictionaries: Resources, Ships and TradeRes.
	Resources = dict:from_list([{'Iron', 10}, {'Food', 10}, {'Gas', 10}]),
	Ships = dict:from_list([{'Cargo ship', 3}, {'Harvester', 3}, {'Escort', 3}]),
	TradeRes = dict:from_list([{'Iron', 0}, {'Food', 0}, {'Gas', 0}]),
	{ok, {Resources, Ships, TradeRes}}.

%% checks if there are enough resources if so detract from resources
%% and reply with ok to build else reply with don't build
handle_call({build, Iron, Food, Gas}, _From, State) ->
	{Res, Ships, Trade} = State,
	I = dict:fetch('Iron', Res),
	F = dict:fetch('Food', Res),
	G = dict:fetch('Gas', Res),
	if 
		I >= Iron andalso F >= Food andalso G >= Gas ->
			TempRes1 = dict:update_counter('Iron', -Iron, Res),
			TempRes2 = dict:update_counter('Food', -Food, TempRes1),
			NewRes = dict:update_counter('Gas', -Gas, TempRes2),
			arbitrator:update_resources(dict:to_list(NewRes)),
			{reply, build_ok, {NewRes, Ships, Trade}};	
		true ->
			{reply, build_nores, State}
	end;	
%% prints the resources and ships available
handle_call(resources, _From, State) ->
	{Resources, Ships, TradeRes} = State,
	io:format("Resources: ~p~n", [dict:to_list(Resources)]),
	io:format("Ships: ~p~n", [dict:to_list(Ships)]),
	io:format("TradeRes: ~p~n", [dict:to_list(TradeRes)]),
	{reply, [], State};
%% starts a harvest and reserves a harvester if one is available. If not it ends the operation
handle_call(start_harvest, _From, State) ->			
	io:format("check if enough ships~n"),
	{Res, Ships, Trade} = State,
	H = dict:fetch('Harvester', Ships),
	if 
		H == 0 -> 
			{reply, noship, State};
		true -> 
			NewShips = dict:update_counter('Harvester', -1, Ships),
			arbitrator:update_ships(dict:to_list(NewShips)),
			{reply, ship, {Res, NewShips, Trade}}
	end;
%% checks if the resources and ships needed for the given trade is available
handle_call({reserve_resource, Type, Qty}, _From, State) ->
	io:format("Check if enough resources~n"),
	{Res, Ships, Trade} = State,
	C = dict:fetch('Cargo ship', Ships),
	if 
		C == 0 -> 
			{reply, noship, State};
		true -> 
			T = dict:fetch(Type, Res),
			if 
				T >= Qty ->
					NewRes = dict:update_counter(Type, -Qty, Res),
					NewShips = dict:update_counter('Cargo ship', -1, Ships),
					NewTrade = dict:update_counter(Type, Qty, Trade),
					arbitrator:update_resources(dict:to_list(NewRes)),
					arbitrator:update_ships(dict:to_list(NewShips)),
					{reply, ok, {NewRes, NewShips, NewTrade}};
				true -> 
					{reply, nores, State}
			end
	end;
handle_call(_Msg, _From, State) ->
	{reply, [], State}.


%% Builds ship of type Type and adds it to Ships, takes random time
handle_cast({building, Type}, State) ->
	io:format("Cast-building: ~w~n", [Type]),
	randomSleep(?MIN_BUILD_TIME, ?MAX_BUILD_TIME),
	{Resources, Ships, Trade} = State,
	NewShips = dict:update_counter(Type, 1, Ships),
	arbitrator:update_ships(dict:to_list(NewShips)),
	io:format("Cast-building: ~w - Done!~n", [Type]),
	{noreply, {Resources, NewShips, Trade}};
%% ends the harvest and increases our current resources accordingly
handle_cast({harvest, Type, Qty}, State) ->
	io:format("harvest cast~n"),
	{Resources, Ships, Trade} = State,
	NewShips = dict:update_counter('Harvester', 1, Ships),
	NewRes = dict:update_counter(Type, Qty, Resources),
	arbitrator:update_ships(dict:to_list(NewShips)),
	arbitrator:update_resources(dict:to_list(NewRes)),
	io:format("~w: ~w~n", [Type, Qty]),
	{noreply, {NewRes, NewShips, Trade}};	
%% receives a message from another player
handle_cast({Node, msg, Msg}, State) ->
	io:format("Message from ~w: ~w~n", [Node, Msg]),
	{noreply, State};
%% receives a trade request from another player
handle_cast({Node, rtrade, {TWant, THave}}, State) ->
	io:format("Trade request from ~w: ~w, ~w~n", [Node, TWant, THave]),
	%TODO: Add request to list of trade requests in GUI
	{noreply, State};
%% receives a trade cancellation from another player
handle_cast({Node, ctrade, {TWant, THave}}, State) ->
	io:format("Cancel request from ~w: ~w, ~w~n", [Node, TWant, THave]),
	%TODO: remove request to list of trade requests in GUI
	{noreply, State};
handle_cast({Node, offer, {TWant, QT, THave, QH}}, State) ->
	io:format("Offer from ~w: ~wx~w for ~wx~w~n", [Node, TWant, QT, THave, QH]),
	%TODO: Update offer list in GUI.
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

