-module(solar_system).
-behavior(gen_server).
	
-export([start_link/0,
		spawner/0,
		print/0,
		harvest/1,
		harvesting/1,
		stop/0,
		connect/1,
		display_nodes/0,
		send/3,
		trade_request/2,
		cancel_request/2, 
		offer/6,
		build/1, 
		build_process/1,
		ship_types/0, 
		resource_types/0,
		set_node_name/1,
		accept_offer/2, 
		cancel_offer/1, 
		transport/3,
		get_number_of_escorts/0,
		get_contacts/0,
		get_outgoing_offers/0,
		get_incoming_offers/0,
		clear_trade_requests/0, 
		destroy_everything/0,
		send_spy_drone/1,
		send_spy_drone_process/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(SERVER, ?MODULE).

%%% Definition of constants 
%% Harvest constants
-define(MAX_HARVEST_METAL, 325).
-define(MAX_HARVEST_RARE, 72).
-define(MAX_HARVEST_TIME, 9000).
-define(MIN_HARVEST_TIME, 2000).

%% Building time constants
-define(MAX_BUILD_TIME, 10000).
-define(MIN_BUILD_TIME, 7000).

%% Transport time constants
-define(MAX_TRANSPORT_TIME, 30000).
-define(MIN_TRANSPORT_TIME, 2000).

%% Factor constants
-define(CARGO_SHIP_FACTOR, 2).
-define(DEATH_RAY_FACTOR, 10).
-define(ESCORT_FACTOR, 4).
-define(HARVESTER_FACTOR, 1).
-define(SPY_FACTOR, 6).

%% Building constants
-define(CARGO_SHIP_METALS, 200).
-define(CARGO_SHIP_WATER, 2).
-define(CARGO_SHIP_CARBON, 3).
-define(CARGO_SHIP_CRYSTALS, 1).
-define(DEATH_RAY_METALS, 15000).
-define(DEATH_RAY_WATER, 1000).
-define(DEATH_RAY_CARBON, 1000).
-define(DEATH_RAY_CRYSTALS, 1000).
-define(ESCORT_METALS, 500).
-define(ESCORT_WATER, 9).
-define(ESCORT_CARBON, 6).
-define(ESCORT_CRYSTALS, 7).
-define(HARVESTER_METALS, 25).
-define(HARVESTER_WATER, 1).
-define(HARVESTER_CARBON, 1).
-define(HARVESTER_CRYSTALS, 2).
-define(SPY_METALS, 800).
-define(SPY_WATER, 21).
-define(SPY_CARBON, 24).
-define(SPY_CRYSTALS, 23).

%% Random function seeds and returns random number from 0 to N
random(N) ->
	%<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	%random:seed(A,B,C),
	random:seed(now()),
	random:uniform(N).

%% Random function seeds and returns random number between N and M
random(N,M) -> 
	%<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	{A, B, C} = now(),
	random:seed(A,B,C),
	%random:seed(now()),
	N + random:uniform(M+1) - 1.

%% Sleep function makes process wait for T milliseconds
sleep(T) ->
	receive
	after T -> true
	end.

%% Sleeps 0 to T
randomSleep(T) ->
	sleep(random(T)).
%% Sleeps N to M
randomSleep(N,M) ->
	sleep(random(N,M)).

%% Starts the solar system Node.	
start_link() ->
	spawn(solar_system, spawner, []),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Stops the solar system Node.
stop() ->
	gen_server:cast(?SERVER, stop).

%% Sets the name of the node.
set_node_name(Name) ->
	%io:format("Setting node name to: ~w~n", [Name]),
	net_kernel:start([Name, longnames]),
	erlang:set_cookie(node(), kaka). 

%% Print server state.
print() ->
	gen_server:call(solar_system, print_state).

%% Outputs list of all resource types
resource_types() ->
	["Metals", "Water", "Carbon", "Crystals"].

%% Outputs list of all ship types
ship_types() ->
	["Escort", "Harvester", "Cargo ship", "Spy Drone"].

%% This function is call by the arbitrator to build
build(Type) ->
	spawn(solar_system, build_process, [Type]).

%% Build function checks the Type of ship and 
%% if there are enough resources to build the ship 
build_process(Type) ->
	SType = atom_to_list(Type),
	if
		Type == 'Death Ray' ->
			Reply = gen_server:call(solar_system, {build, ?DEATH_RAY_METALS, ?DEATH_RAY_WATER, ?DEATH_RAY_CARBON, ?DEATH_RAY_CRYSTALS}),
			if
				Reply == build_ok ->
					building(Type),
					arbitrator:built_death_ray();
				true ->
					arbitrator:format("Not enough resources~n", []),
					arbitrator:format("DEATH RAY: ~p Metals, ~p Water, ~p Carbon, ~p Crystals~n", [?DEATH_RAY_METALS, ?DEATH_RAY_WATER, ?DEATH_RAY_CARBON, ?DEATH_RAY_CRYSTALS])
			end;
		Type == 'Harvester' ->
			Reply = gen_server:call(solar_system, {build, ?HARVESTER_METALS, ?HARVESTER_WATER, ?HARVESTER_CARBON, ?HARVESTER_CRYSTALS}),
			if
				Reply == build_ok ->
					building(Type);
				true ->
					arbitrator:format("Not enough resources~n", []),
					arbitrator:format("Harvester: ~p Metals, ~p Water, ~p Carbon, ~p Crystals~n", [?HARVESTER_METALS, ?HARVESTER_WATER, ?HARVESTER_CARBON, ?HARVESTER_CRYSTALS])
			end;
		Type == 'Cargo ship' ->
			Reply = gen_server:call(solar_system, {build, ?CARGO_SHIP_METALS, ?CARGO_SHIP_WATER, ?CARGO_SHIP_CARBON, ?CARGO_SHIP_CRYSTALS}),
			if
				Reply == build_ok ->
					building(Type);
				true ->
					arbitrator:format("Not enough resources~n", []),
					arbitrator:format("Cargo ship: ~p Metals, ~p Water, ~p Carbon, ~p Crystals~n", [?CARGO_SHIP_METALS, ?CARGO_SHIP_WATER, ?CARGO_SHIP_CARBON, ?CARGO_SHIP_CRYSTALS])
			end;
		Type == 'Escort' ->
			Reply = gen_server:call(solar_system, {build, ?ESCORT_METALS, ?ESCORT_WATER, ?ESCORT_CARBON, ?ESCORT_CRYSTALS}),
			if
				Reply == build_ok ->
					building(Type);
				true ->
					arbitrator:format("Not enough resources~n", []),
					arbitrator:format("Escort ship: ~p Metals, ~p Water, ~p Carbon, ~p Crystals~n", [?ESCORT_METALS, ?ESCORT_WATER, ?ESCORT_CARBON, ?ESCORT_CRYSTALS])
			end;
		Type == 'Spy drone' ->
			Reply = gen_server:call(solar_system, {build, ?SPY_METALS, ?SPY_WATER, ?SPY_CARBON, ?SPY_CRYSTALS}),
			if
				Reply == build_ok ->
					building(Type);
				true ->
					arbitrator:format("Not enough resources~n", []),
					arbitrator:format("Spy drone: ~p Metals, ~p Water, ~p Carbon, ~p Crystals~n", [?SPY_METALS, ?SPY_WATER, ?SPY_CARBON, ?SPY_CRYSTALS])
			end;
		true ->
			arbitrator:format("ERROR:213 - Unkown Type: ~s", [SType]),
			false
	end.

%% Building function sleeps for the time it takes to 
%% build ship of type Type and takes into account its building factor
building(Type) ->
	SType = atom_to_list(Type),
	arbitrator:format("Building: ~s~n", [SType]),
	if
		Type == 'Cargo ship' ->
			randomSleep(?MIN_BUILD_TIME * ?CARGO_SHIP_FACTOR, ?MAX_BUILD_TIME * ?CARGO_SHIP_FACTOR);
		Type == 'Death Ray' ->
			randomSleep(?MIN_BUILD_TIME * ?DEATH_RAY_FACTOR, ?MAX_BUILD_TIME * ?DEATH_RAY_FACTOR);
		Type == 'Escort' ->
			randomSleep(?MIN_BUILD_TIME * ?ESCORT_FACTOR, ?MAX_BUILD_TIME * ?ESCORT_FACTOR);
		Type == 'Harvester' ->
			randomSleep(?MIN_BUILD_TIME * ?HARVESTER_FACTOR, ?MAX_BUILD_TIME * ?HARVESTER_FACTOR);
		Type == 'Spy drone' ->
			randomSleep(?MIN_BUILD_TIME * ?SPY_FACTOR, ?MAX_BUILD_TIME * ?SPY_FACTOR);
		true ->
			arbitrator:format("ERROR:214 - Unkown Type: ~p", [SType])
	end,
	gen_server:cast(solar_system, {building, Type}),
	arbitrator:format("Done building: ~s~n", [SType]).

% Start a harvesting operation on a location of type 'Type'
% If no harvesters are available, nothing happens
% If resource does not exist in system, nothing happens
harvest(Type) ->
	IsResource = lists:member(Type, ['Metals', 'Water', 'Carbon', 'Crystals']) or (Type == 'Rare'),
	if IsResource == true ->
		%io:format("Harvest~n"),
		{Reply, NType} = gen_server:call(solar_system, {start_harvest, Type}),
		%io:format("reply: ~p~n", [Reply]),
		if
			Reply == badResource -> 
				arbitrator:format("There is no ~s in this solar system~n", [NType]);
			Reply == ship ->
				spawn(solar_system, harvesting, [NType]);
			true ->
				false
		end;
	true -> arbitrator:format("~s is not a resource ~n", [Type])
	end.

% Perform a harvesting operation of the given type and after waiting for  
% some time, sends the result to the server if the harvester has not been attacked by pirates
harvesting(Type) ->
	%io:format("Harvesting~n"),
	PirateAttack = attacked_by_pirates(3),
	if PirateAttack == 0 ->
			randomSleep(?MIN_HARVEST_TIME, ?MAX_HARVEST_TIME),
			gen_server:cast(solar_system, {harvest_plundered, atom_to_list(Type)});
		true ->
			randomSleep(?MIN_HARVEST_TIME, ?MAX_HARVEST_TIME),
		if
			Type == 'Metals' ->
				gen_server:cast(solar_system, {harvest, Type, random:uniform(?MAX_HARVEST_METAL)});
			true ->
				gen_server:cast(solar_system, {harvest, Type, random:uniform(?MAX_HARVEST_RARE)})
		end
	end.

%% Death Ray activated send to all nodes reset of gathered resources and docked ships
destroy_everything() ->
	gen_server:cast(solar_system, deathray).

%% Send to all nodes a valid trade request
trade_request(TWant, THave) ->
	IsResource = lists:member(TWant, ['Metals', 'Water', 'Carbon', 'Crystals']) and lists:member(THave, ['Metals', 'Water', 'Carbon', 'Crystals']),
	if IsResource == true ->
		arbitrator:format("Broadcasting need for ~s, offering ~s~n", [TWant,THave]),
		Fun = fun(N) -> send(rtrade, {TWant, THave}, N) end,
		lists:foreach(Fun, nodes());
	true -> arbitrator:format("Not a valid resource~n", [])
	end.

%% Cancel trade requests sent to all nodes
cancel_request(TWant, THave) ->
	Fun = fun(N) -> send(ctrade, {TWant, THave}, N) end,
	lists:foreach(Fun, nodes()).	

%% Cancels an offer sent to 'Node'
cancel_offer(Node) ->
	gen_server:cast(solar_system, {cOutOffer, Node}),
	send(coffer, {}, Node).

%% Checks if offer is possible, that is:
%% are there any cargo ships for this mission,
%% are there enough escort ships for this mission, 
%% are there enough resources for this mission,
%% If these requirements are satisfied then send offer to Node
offer(Node, TWant, QT, THave, QH, NumberOfEscorts) ->
	%io:format("Offer~n"),	
	HasOffer = gen_server:call(solar_system, {have_offer_to, Node}),
	%io:format("HasOffer: ~p~n", [HasOffer]),
	if
		HasOffer =/= true ->
			Reply = gen_server:call(solar_system, {reserve_resource, THave, QH, NumberOfEscorts}),
			if 
				Reply == noship ->
					arbitrator:format("There are no available Cargo ships for this mission!~n", []),
					{ok, Reply};
				Reply == noescort ->
					arbitrator:format("There are not enough Escort ships for this mission!~n", []),
					{ok, Reply};
				Reply == nores ->
					arbitrator:format("There are not enough resources for this mission!~n", []),
					{ok, Reply};
				true ->
					arbitrator:format("Offer sent to ~s: ~s ~p for ~s ~p~n", [Node, THave, QH, TWant, QT]),
					send(offer, {TWant, QT, THave, QH, NumberOfEscorts}, Node),
					gen_server:cast(solar_system, {Node, outoffer, {TWant, QT, THave, QH, NumberOfEscorts}})
			end;
		true ->
			arbitrator:format("Outstanding offer to ~s present.~n", [Node])
	end.

%% Accept offer from Node if possible.	
accept_offer(Node, NumberOfEscorts) ->
	% First check if resources are available
	io:format("Are resources available?~n"),
	{THave, Qty, _, _, _} = gen_server:call(solar_system, {get_offer_from, Node}),
	
	Reply = gen_server:call(solar_system, {reserve_resource, THave, Qty, NumberOfEscorts}),
	if
		Reply == noship ->
			arbitrator:format("There are no available Cargo ships for this mission!~n", []),
			{ok, Reply};
		Reply == noescort ->
			arbitrator:format("There are not enough Escort ships for this mission!~n", []),
			{ok, Reply};
		Reply == nores ->
			arbitrator:format("There are not enough resources for this mission!~n", []),
			{ok, Reply};
		true ->
			io:format("Accepting offer from ~p~n", [Node]),
			ReplyFromOther = sendWait(accept_offer, [], Node, 5000),
			if
				ReplyFromOther == confirm ->
					%sleep, need to spawn if so.
					gen_server:cast(solar_system, {offer_confirmed, Node, NumberOfEscorts});
				true ->
					gen_server:cast(solar_system, {offer_cancelled, Node})
			end
	end.

%% Returns number of available escorts
get_number_of_escorts() ->
	gen_server:call(solar_system, get_number_of_escorts).

%% Returns a list of nodes we have made contact with.
get_contacts() ->
	gen_server:call(solar_system, get_contacts).

%% Returns a list of offers we have made to other nodes.
get_outgoing_offers() ->
	gen_server:call(solar_system, get_outgoing_offers).
	
%% Returns a list of offers made to us.
get_incoming_offers() ->
	gen_server:call(solar_system, get_incoming_offers).

%% Cleans out the trade requests dictionary
clear_trade_requests() ->
	gen_server:cast(solar_system, clear_trade_requests).
	
%% Sends spy drone to Node
send_spy_drone(Node) ->
	spawn(solar_system, send_spy_drone_process, [Node]).
	
send_spy_drone_process(Node) ->
	Reply = gen_server:call(solar_system, reserve_drone),
	if
		Reply == nodrone ->
			arbitrator:format("There are no available spy drones for this mission!~n", []);
		true ->
			arbitrator:format("Deploying spy drone to ~p~n", [Node]),
			transport_delay(),
			{Res, Ships} = sendWait(spy, [], Node, 5000),
			transport_delay(),
			arbitrator:format("~s resources: ~w~n", [Node, dict:to_list(Res)]),
			arbitrator:format("~s ships: ~w~n", [Node, dict:to_list(Ships)]),
			gen_server:cast(solar_system, return_drone),
			ok
	end.
	
%% Spawns resource planets in to solar system	
spawner() -> 
	io:format("Spawner~n").

%%% Network functions 

%% Connects to Node
connect(Node) ->
	net_kernel:connect_node(Node).

%% Displays all nodes you are connected to
display_nodes() ->
	nodes().	

%% Sends message of type Type to Node 
send(Type, Msg, Node) ->
	gen_server:cast({solar_system, Node}, {node(), Type, Msg}).

sendWait(Type, Msg, Node, Time) ->	
	gen_server:call({solar_system, Node}, {node(), Type, Msg}, Time).
	
%%% gen_server callbacks

init([]) ->		
	% The state consists of 5 dictionaries: 
	% Resources: available resources
	% Ships: available ships
	% TradeRes: resources reserved for active offers
	% Requests: Trade requests from other nodes
	% Offers: Offers from other nodes
	% OutOffers: Offers to other nodes
	% Contacts: Nodes we have made contact with
	% DR: whether we have a death ray or not
	% System: what resources this system has
	
	Ships = dict:from_list(lists:reverse([{'Cargo ship', 5}, {'Harvester', 10}, {'Escort', 2}, {'Spy drone', 1}])),
	TradeRes = dict:from_list([{'Metals', 0}, {'Water', 0}, {'Carbon', 0}, {'Crystals', 0}]),
	Requests = dict:from_list([]),
	Offers = dict:from_list([]),
	OutOffers = dict:from_list([]),
	Contacts = dict:from_list([]),
	DR = false,

	ResourceType = random(0, 2),
	if ResourceType == 0 ->
		   Resources = dict:from_list([{'Metals', 325}, {'Water', 10}, {'Carbon', 14}, {'Crystals', 25}]),
		   FirstResource = 'Water',
		   SecondResource = 'Carbon';
	   ResourceType == 1 ->
		   Resources = dict:from_list([{'Metals', 325}, {'Water', 10}, {'Carbon', 24}, {'Crystals', 15}]),
		   FirstResource = 'Water',
		   SecondResource = 'Crystals';
	   ResourceType == 2 ->
		   Resources = dict:from_list([{'Metals', 325}, {'Water', 20}, {'Carbon', 14}, {'Crystals', 15}]),
		   FirstResource = 'Carbon',
		   SecondResource = 'Crystals'
	end,
	arbitrator:update_ships(dict:to_list(Ships)),
	arbitrator:update_resources(dict:to_list(Resources)),
	arbitrator:format("This solar system contains ~s and ~s~n", [atom_to_list(FirstResource), atom_to_list(SecondResource)]),
	{ok, {Resources, Ships, TradeRes, Requests, Offers, OutOffers, Contacts, DR, [FirstResource, SecondResource]}}.

%% checks if there are enough resources if so detract from resources
%% and reply with ok to build else reply with don't build
handle_call({build, Metals, Water, Carbon, Crystals}, _From, State) ->
	{Res, Ships, Trade, Req, Off, Out, Con, DR, System} = State,
	I = dict:fetch('Metals', Res),
	F = dict:fetch('Water', Res),
	G = dict:fetch('Carbon', Res),
	H = dict:fetch('Crystals', Res),
	if 
		I >= Metals andalso F >= Water andalso G >= Carbon  andalso H >= Crystals->
			TempRes1 = dict:update_counter('Metals', -Metals, Res),
			TempRes2 = dict:update_counter('Water', -Water, TempRes1),
			TempRes3 = dict:update_counter('Crystals', -Crystals, TempRes2),

			NewRes = dict:update_counter('Carbon', -Carbon, TempRes3),
			arbitrator:update_resources(dict:to_list(NewRes)),
			{reply, build_ok, {NewRes, Ships, Trade, Req, Off, Out, Con, DR, System}};	
		true ->
			{reply, build_nores, State}
	end;	
%% prints the server state
handle_call(print_state, _From, State) ->
	io:format("State is: ~p~n", [State]),
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR, System} = State,
	io:format("Resources: ~p~n", [dict:to_list(Res)]),
	io:format("Ships: ~p~n", [dict:to_list(Ships)]),
	io:format("TradeRes: ~p~n", [dict:to_list(TradeRes)]),
	io:format("Trade requests: ~p~n", [dict:to_list(Req)]),
	io:format("Trade offers: ~p~n", [dict:to_list(Off)]),
	io:format("Outgoing trade offers: ~p~n", [dict:to_list(Out)]),
	io:format("Contacts: ~p~n", [dict:to_list(Con)]),
	io:format("Death Ray: ~p~n", [DR]),
	io:format("System Type is ~p~n", [System]),
	{reply, [], State};
%% starts a harvest and reserves a harvester if one is available. If not it ends the operation
handle_call({start_harvest, Type}, _From, State) ->			
	io:format("check if enough ships~n"),
	{Res, Ships, Trade, Req, Off, Out, Con, DR, System} = State,
	Pred = lists:member(Type, System),
	if Pred; Type == 'Metals'; Type == 'Rare' ->
		H = dict:fetch('Harvester', Ships),
		if 
			H == 0 -> 
				{reply, {noship, Type}, State};
			true -> 
					NewShips = dict:update_counter('Harvester', -1, Ships),
					arbitrator:update_ships(dict:to_list(NewShips)),
				io:format("Type is ~p~n", [Type]),
				if 
					% metals
					Type =/= 'Rare' -> 
						{reply, {ship, Type}, {Res, NewShips, Trade, Req, Off, Out, Con, DR, System}};
					% rare resource
					true ->
						Chosen = random_member(System),
						{reply, {ship, Chosen}, {Res, NewShips, Trade, Req, Off, Out, Con, DR, System}}
				end
		end;
	true ->
		{reply, {badResource, Type}, State}
	end;
%% checks if the resources and ships needed for the given trade is available
handle_call({reserve_resource, Type, Qty, NumberOfEscorts}, _From, State) ->
	io:format("Check if enough resources~n"),
	{Res, Ships, Trade, Req, Off, Out, Con, DR, System} = State,
	C = dict:fetch('Cargo ship', Ships),
	E = dict:fetch('Escort', Ships),
	if 
		C == 0 -> 
			{reply, noship, State};
		E < NumberOfEscorts -> 
			{reply, noescort, State};
		true -> 
			T = dict:fetch(Type, Res),
			if 
				T >= Qty ->
					NewRes = dict:update_counter(Type, -Qty, Res),
					TempShips = dict:update_counter('Cargo ship', -1, Ships),
					NewShips = dict:update_counter('Escort', -NumberOfEscorts, TempShips),
					NewTrade = dict:update_counter(Type, Qty, Trade),
					arbitrator:update_resources(dict:to_list(NewRes)),
					arbitrator:update_ships(dict:to_list(NewShips)),
					{reply, ok, {NewRes, NewShips, NewTrade, Req, Off, Out, Con, DR, System}};
				true -> 
					{reply, nores, State}
			end
	end;
%% Returns offer from Node
handle_call({get_offer_from, Node}, _From, State) ->
	{_, _, _, _, Off, _, _, _, _} = State,
	[Offer] = dict:fetch(Node, Off),
	{reply, Offer, State};
%% Returns true if there is an offer from Node 
handle_call({have_offer_to, Node}, _From, State) ->
	{_, _, _, _, _, Out, _, _, _} = State,
	Reply = dict:is_key(Node, Out),
	{reply, Reply, State};
%% Check if the key Node exists in out offers, if so confirm trade, otherwise cancel 
handle_call({Node, accept_offer, _Msg}, _From, State) ->
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR, System} = State,
	ContainsNode = dict:is_key(Node, Out),
	if
		ContainsNode == true ->
			[{TGot, QG, THad, QH, NumberOfEscorts}] = dict:fetch(Node, Out),
			
			spawn(solar_system, transport, [TGot, QG, NumberOfEscorts]),
			
			%% Update dictionaries
			NewOut = dict:erase(Node, Out), 
			NewTradeRes = dict:update_counter(THad, -QH, TradeRes),
			
			{reply, confirm, {Res, Ships, NewTradeRes, Req, Off, NewOut, Con, DR, System}};
		true ->
			{reply, cancel, State}
	end;
%% Returns the number of escorts you have 
handle_call(get_number_of_escorts, _From, State) ->
	{_, Ships, _, _, _, _, _, _, _} = State,
	Nescorts = dict:fetch('Escort', Ships),
	{reply, Nescorts, State};
%% Returns all your contacts
handle_call(get_contacts, _From, State) ->
	{_, _, _, _, _, _, Con, _, _} = State,
	Contacts = dict:fetch_keys(Con),
	{reply, Contacts, State};
%% Returns all outgoing offers
handle_call(get_outgoing_offers, _From, State) ->
	{_, _, _, _, _, Out, _, _, _} = State,
	{reply, Out, State};
%% Returns all incoming offers
handle_call(get_incoming_offers, _From, State) ->
	{_, _, _, _, Off, _, _, _, _} = State,
	{reply, Off, State};
handle_call(reserve_drone, _From, State) ->
	io:format("Reserve drone if available~n"),
	{Res, Ships, Trade, Req, Off, Out, Con, DR, System} = State,
	S = dict:fetch('Spy drone', Ships),
	if 
		S == 0 -> 
			{reply, nodrone, State};
		true -> 
			NewShips = dict:update_counter('Spy drone', -1, Ships),
			arbitrator:update_ships(dict:to_list(NewShips)),
			{reply, ok, {Res, NewShips, Trade, Req, Off, Out, Con, DR, System}}
	end;
handle_call({_Node, spy, _Msg}, _From, State) ->
	%% Check if the key Node exists in out offers, if so confirm trade, otherwise cancel
	{Res, Ships, _, _, _, _, _, _, _} = State,
	{reply, {Res, Ships}, State};
handle_call(_Msg, _From, State) ->
	{reply, [], State}.


%% Builds ship of type Type and adds it to Ships, takes random time
handle_cast({building, Type}, State) ->
	%io:format("Cast-building: ~w~n", [Type]),
	{Resources, Ships, Trade, Req, Off, Out, Con, DR, System} = State,
	
	NewShips = dict:update_counter(Type, 1, Ships),
	arbitrator:update_ships(dict:to_list(NewShips)),
	%io:format("Cast-building: ~w - Done!~n", [Type]),
	if 
		Type == 'Death Ray' ->
			{noreply, {Resources, NewShips, Trade, Req, Off, Out, Con, true, System}};
		true ->
			{noreply, {Resources, NewShips, Trade, Req, Off, Out, Con, DR, System}}
	end;

handle_cast({harvest_plundered, Type}, State) ->
	RandLost = ["lost", "plundered", "destroyed", "hijacked", "disabled", "fried", "stolen", "attacked", "taken hostage", "blown up", "blown to smithereens", "attacked by pirates", "shot at by sharks with freaking laser beams attached to their heads", "attacked by sharks with lasers", "fired at with lasers", "bombed", "lost to radiation", "irradiated", "lost to electromagnetic storms", "disabled by a stray torpedo", "lost to pilot error"],
	Index = random:uniform(length(RandLost)),
	Reason = lists:nth(Index, RandLost),
	arbitrator:format("A harvester was ~s while on a mission for ~s ~n", [Reason, Type]),
	{noreply, State};
%% ends the harvest and increases our current resources accordingly
handle_cast({harvest, Type, Qty}, State) ->
	io:format("harvest cast~n"),
	%io:format("State is: ~p~n", [State]),
	{Resources, Ships, Trade, Req, Off, Out, Con, DR, System} = State,
	NewShips = dict:update_counter('Harvester', 1, Ships),
	NewRes = dict:update_counter(Type, Qty, Resources),
	arbitrator:update_ships(dict:to_list(NewShips)),
	arbitrator:update_resources(dict:to_list(NewRes)),

	io:format("~w: ~w~n", [Type, Qty]),
	{noreply, {NewRes, NewShips, Trade, Req, Off, Out, Con, DR, System}};	
%% receives a message from another player
handle_cast({Node, msg, Msg}, State) ->
	{Resources, Ships, Trade, Req, Off, Out, Con, DR, System} = State,
	NewCon = dict:store(Node, 0, Con),
	arbitrator:format("!!! Private message from ~s: ~s !!!~n", [Node, Msg]),
	{noreply, {Resources, Ships, Trade, Req, Off, Out, NewCon, DR, System}};
handle_cast({_Node, deathray, {}}, State) ->
	io:format("You have been destroyed by the death ray :(~n"),
	{_, _, _, Req, Off, Out, Con, _, System} = State,
	NewRes = dict:from_list([{'Metals', 0}, {'Water', 0}, {'Carbon', 0}, {'Crystals', 0}]),
	NewShips = dict:from_list([{'Cargo ship', 0}, {'Harvester', 0}, {'Escort', 0}]),
	NewTradeRes = dict:from_list([{'Metals', 0}, {'Water', 0}, {'Carbon', 0}, {'Crystals', 0}]),
	arbitrator:update_resources(dict:to_list(NewRes)),
	arbitrator:update_ships(dict:to_list(NewShips)),
	{noreply, {NewRes, NewShips, NewTradeRes, Req, Off, Out, Con, false, System}};
%% receives a trade request from another player
handle_cast({Node, rtrade, {TWant, THave}}, State) ->
	io:format("Trade request from ~w: ~w, ~w~n", [Node, TWant, THave]),
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR, System} = State,
	Fun = fun(Old) -> [{TWant, THave}] ++ Old -- [{TWant, THave}] end,
	NReq = dict:update(Node, Fun, [{TWant, THave}], Req),
	NewCon = dict:store(Node, 0, Con),		
	arbitrator:update_trade_requests(NReq),
	{noreply, {Res, Ships, TradeRes, NReq, Off, Out, NewCon, DR, System}};
%% receives a trade cancellation from another player
handle_cast({Node, ctrade, {TWant, THave}}, State) ->
	io:format("Cancel request from ~w: ~w, ~w~n", [Node, TWant, THave]),
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR, System} = State,
	Fun = fun(Old) -> Old -- [{TWant, THave}] end,
	NReq = dict:update(Node, Fun, [{TWant, THave}], Req),
	arbitrator:update_trade_requests(NReq),
	{noreply, {Res, Ships, TradeRes, NReq, Off, Out, Con, DR, System}};
handle_cast({Node, offer, {TWant, QT, THave, QH, NumberOfEscorts}}, State) ->
	io:format("Offer from ~w: ~wx~w for ~wx~w, escorts: ~w~n", [Node, TWant, QT, THave, QH, NumberOfEscorts]),
	%io:format("State is: ~p~n", [State]),
	%TODO: Update offer list in GUI.
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR, System} = State,
	Fun = fun(Old) -> Old end,
	NOff = dict:update(Node, Fun, [{TWant, QT, THave, QH, NumberOfEscorts}], Off),
	arbitrator:update_offers(NOff),
	NewCon = dict:store(Node, 0, Con),
	{noreply, {Res, Ships, TradeRes, Req, NOff, Out, NewCon, DR, System}};
%% receive an offer cancellation
handle_cast({cOutOffer, Node}, State) ->
	io:format("Cancel our own offer ~n"),
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR, System} = State,
	[{_, _, THave, Qt, NumberOfEscorts}] = dict:fetch(Node, Out),
	TShips = dict:update_counter('Cargo ship', 1, Ships),
	NShips = dict:update_counter('Escort', NumberOfEscorts, TShips),
	NOut = dict:erase(Node, Off),
	NRes = dict:update_counter(THave, Qt, Res),
	NTradeRes = dict:update_counter(THave, -Qt, TradeRes),

	arbitrator:update_resources(dict:to_list(NRes)),
	arbitrator:update_ships(dict:to_list(NShips)),

	{noreply, {NRes, NShips, NTradeRes, Req, Off, NOut, Con, DR, System}};
%% removes a trade offer from 'Node' from our list
handle_cast({Node, coffer, _}, State) ->
	io:format("Remove cancelled offer~n"),
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR, System} = State,
	NOff = dict:erase(Node, Off),
	arbitrator:update_offers(NOff),
	{noreply, {Res, Ships, TradeRes, Req, NOff, Out, Con, DR, System}};
%% adds an outgoing offer to the list
handle_cast({Node, outoffer, {TWant, QT, THave, QH, NumberOfEscorts}}, State) ->
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR, System} = State,
	Fun = fun(Old) -> Old end,
	NOut = dict:update(Node, Fun, [{TWant, QT, THave, QH, NumberOfEscorts}], Out),
	{noreply, {Res, Ships, TradeRes, Req, Off, NOut, Con, DR, System}};	
%% when an offer has been accepted, removes it from the list and starts the transport process
handle_cast({offer_confirmed, Node, NumberOfEscorts}, State) ->
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR, System} = State,
	[{THad, QH, TGot, QG, _}] = dict:fetch(Node, Off),
	
	spawn(solar_system, transport, [TGot, QG, NumberOfEscorts]),
	
	%% Update dictionaries
	NewOff = dict:erase(Node, Off), 
	NewTradeRes = dict:update_counter(THad, -QH, TradeRes),

	arbitrator:update_offers(NewOff),
	
	{noreply, {Res, Ships, NewTradeRes, Req, NewOff, Out, Con, DR, System}};
%% when an offer we have prepared has been cancelled, we remove it from the list
%% and add the reserved resources and ships back to our pool
handle_cast({offer_cancelled, Node}, State) ->
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR, System} = State,
	[{THad, QH, _, _, _}] = dict:fetch(Node, Off),
	
	%% Update dictionaries
	NewOff = dict:erase(Node, Off), 
	NewShips = dict:update_counter('Cargo ship', 1, Ships),
	NewRes = dict:update_counter(THad, QH, Res),
	NewTradeRes = dict:update_counter(THad, -QH, TradeRes),
	
	arbitrator:update_ships(dict:to_list(NewShips)),
	arbitrator:update_resources(dict:to_list(NewRes)),
	arbitrator:update_offers(NewOff),
	
	{noreply, {NewRes, NewShips, NewTradeRes, Req, NewOff, Out, Con, DR, System}};
%% informs the player of a lost transport
handle_cast({transport_lost}, State) ->
	io:format("A transport was lost."),
	Msg = "It's been a while since a trade mission started and the cargo ship has not returned. Perhaps it has been lost?",
	T = random(10000, 900000),
	arbitrator:lost_cargo(T, Msg),
	{noreply, State};
%% updates our resources and ships after a successful transportation
handle_cast({transport_done, Type, Qt, NumberOfEscorts}, State) ->
	io:format("Gen_server: transport is done ~n This function should update the resources instead: ~p ~p ~n", [Type, Qt]),
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR, System} = State,
	TempShips = dict:update_counter('Cargo ship', 1, Ships),
	NewShips = dict:update_counter('Escort', NumberOfEscorts, TempShips),
	NewRes = dict:update_counter(Type, Qt, Res),
	arbitrator:update_ships(dict:to_list(NewShips)),
	arbitrator:update_resources(dict:to_list(NewRes)),
	{noreply, {NewRes, NewShips, TradeRes, Req, Off, Out, Con, DR, System}};
%% empties our trade request list
handle_cast(clear_trade_requests, State) ->
	{Res, Ships, TradeRes, _, Off, Out, Con, DR, System} = State,
	NewReq = dict:from_list([]),
	arbitrator:update_trade_requests(NewReq),
	{noreply, {Res, Ships, TradeRes, NewReq, Off, Out, Con, DR, System}};
%% If we have a deatray we cast a deathray message to each other node.
handle_cast(deathray, State) ->
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR, System} = State,
	if DR == true ->
		arbitrator:format("Activating Death Ray~n", []),
		Fun = fun(N) -> 
			arbitrator:format("Terminating ~s~n", [N]),
			send(deathray, {}, N) 
		end,
		lists:foreach(Fun, nodes()),
		{noreply, {Res, Ships, TradeRes, Req, Off, Out, Con, false, System}};
	true -> 
		arbitrator:format("You need to build Death Ray first~n", []),
		{noreply, State}
	end;
%% Handles return of spy drone, increments the Spy drone counter if
%% it is not attacked by pirates.
handle_cast(return_drone, State) ->
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR, System} = State,
	Drone_Broke = attacked_by_pirates(3),
	if Drone_Broke == 0 ->
		NewShips = dict:update_counter('Spy drone', 1, Ships),
		arbitrator:update_ships(dict:to_list(NewShips));
	   true -> NewShips = Ships
	end,
	{noreply, {Res, NewShips, TradeRes, Req, Off, Out, Con, DR, System}};
%% Handles stop cast send to server.
handle_cast(stop, State) ->
	io:format("Stopping solar_system ~n"),
	{stop, normal, State}.
	
%% Handles timeouts and messages sent to gen_server without cast/call.
handle_info(Info, State) ->
	io:format("~p~n", [Info]),
	{noreply, State}.

%% Called when the server terminates normally.
terminate(normal, _State) ->
	ok.

%% Unused, for gen_server behaviour
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Sleep for a random amount of time to simulate transport travel time.
transport_delay() ->
	randomSleep(?MIN_TRANSPORT_TIME, ?MAX_TRANSPORT_TIME).

% pirate attacks
attacked_by_pirates(NumberOfEscorts) ->
	Pirates = random(0, 100), % determine whether or not the pirates are going to actually engage
	Strength = random(0, 3), % how strong the pirates will be, if they engage

	% chance of pirate attack is quite low, but they are more likely to attack if they are max strength
	if Pirates > 85, Strength > NumberOfEscorts; Strength == 3 ->
		RemainingEscorts = lists:max([0, NumberOfEscorts - Strength]);
	   Pirates > 50, Strength == NumberOfEscorts; Strength == 3 ->
		   RemainingEscorts = lists:max([0, NumberOfEscorts - 1]);
	   true -> RemainingEscorts = NumberOfEscorts
	end,
	RemainingEscorts.

% whether to get targeted by pirates
found_by_pirates(E, Q) ->
	Threshhold = random(0, 100),
	Size = random(0, 25),
	if Threshhold > Q, Size > E ->
		true;
	true -> false
	end.
	
% Transport Qt amount of item Type with NumberOfEscorts escorts 
transport(Type, Qt, NumberOfEscorts) -> 
	arbitrator:format("Retrieving ~p x ~s, escorted by a team of size ~p ~n", [Qt, Type, NumberOfEscorts]), % notify the user that the mission is starting
	if NumberOfEscorts =/= 0 ->
		   Escorts = attacked_by_pirates(NumberOfEscorts), % if there are escorts, they might encounter pirates on the way out
		   transport_delay(); % wait for escorts to arrive
	true -> Escorts = NumberOfEscorts
	end,
	TargetedByPirates = found_by_pirates(Escorts, Qt), % after they retrieve the cargo, the pirates might be interested in plundering
	if TargetedByPirates -> % the pirates are interesetd in plundering, they might attack again
		RemainingEscorts = attacked_by_pirates(Escorts);
		true -> RemainingEscorts = Escorts
	end,
	if RemainingEscorts == 0, TargetedByPirates ->
		gen_server:cast(solar_system, {transport_lost}); % cast that the transport was lost
	   true -> % the transport survives the trip, delay it and return the items
		transport_delay(),
		arbitrator:format("Transport team has arrived, bringing ~p x ~s! ~n", [Qt, Type]),
		gen_server:cast(solar_system, {transport_done, Type, Qt, RemainingEscorts})
	end.


% Utility function to pick a random item from a list
random_member(L) -> 
	Index = random:uniform(length(L)),
	lists:nth(Index, L).

