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
		send_spy_drone/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define(SERVER, ?MODULE).

-define(MAX_HARVEST_METAL, 15).
-define(MAX_HARVEST_RARE, 3).
-define(MAX_HARVEST, 1000).
-define(MAX_HARVEST_TIME, 4000).
-define(MIN_HARVEST_TIME, 2000).

-define(MAX_BUILD_TIME, 10000).
-define(MIN_BUILD_TIME, 7000).

-define(MAX_SPY_TIME, 5000).
-define(MIN_SPY_TIME, 2000).

-define(CARGO_SHIP_FACTOR, 2).
-define(DEATH_RAY_FACTOR, 10).
-define(ESCORT_FACTOR, 4).
-define(HARVESTER_FACTOR, 1).
-define(SPY_FACTOR, 1).

-define(CARGO_SHIP_METALS, 200).
-define(CARGO_SHIP_WATER, 2).
-define(CARGO_SHIP_CARBON, 3).
-define(DEATH_RAY_METALS, 1000).
-define(DEATH_RAY_WATER, 50).
-define(DEATH_RAY_CARBON, 50).
-define(ESCORT_METALS, 500).
-define(ESCORT_WATER, 9).
-define(ESCORT_CARBON, 5).
-define(HARVESTER_METALS, 50).
-define(HARVESTER_WATER, 1).
-define(HARVESTER_CARBON, 1).
-define(SPY_METALS, 300).
-define(SPY_WATER, 2).
-define(SPY_CARBON, 5).

random(N) ->
	%<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	%random:seed(A,B,C),
	random:seed(now()),
	random:uniform(N).

random(N,M) -> 
	%<<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	{A, B, C} = now(),
	random:seed(A,B,C),
	%random:seed(now()),
	N + random:uniform(M+1) - 1.

sleep(T) ->
	receive
	after T -> true
	end.

randomSleep(T) ->
	sleep(random(T)).
randomSleep(N,M) ->
	sleep(random(N,M)).

%% Starts the solar system Node.	
start_link() ->
	spawn(solar_system, spawner, []),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Stops the solar system Node.
stop() ->
    gen_server:cast(?SERVER, stop).

%% Sets the Nodes name.
set_node_name(Name) ->
	%io:format("Setting node name to: ~w~n", [Name]),
	net_kernel:start([Name, longnames]),
	erlang:set_cookie(node(), kaka). 

%% Print server state.
print() ->
	gen_server:call(solar_system, print_state).

resource_types() ->
	["Metals", "Water", "Carbon"].

ship_types() ->
	["Escort", "Harvester", "Cargo ship"].

%% This function is call by the arbitrator to build
build(Type) ->
	spawn(solar_system, build_process, [Type]).

%% Build function checks the Type of ship and 
%% if there are enough resources to build the ship 
build_process(Type) ->
	SType = atom_to_list(Type),
	%arbitrator:format("Build: ~p ~n", [SType]),
	if
		Type == 'Death Ray' ->
			Reply = gen_server:call(solar_system, {build, ?DEATH_RAY_METALS, ?DEATH_RAY_WATER, ?DEATH_RAY_CARBON}),
			if
				Reply == build_ok ->
					building(Type),
					arbitrator:built_death_ray();
				true ->
					arbitrator:format("Not enough resources~n", []),
					arbitrator:format("DEATH RAY: ~p Metals, ~p Water, ~p Carbon~n", [?DEATH_RAY_METALS, ?DEATH_RAY_WATER, ?DEATH_RAY_CARBON])
			end;
		Type == 'Harvester' ->
			Reply = gen_server:call(solar_system, {build, ?HARVESTER_METALS, ?HARVESTER_WATER, ?HARVESTER_CARBON}),
			if
				Reply == build_ok ->
					building(Type);
				true ->
					arbitrator:format("Not enough resources~n", []),
					arbitrator:format("Harvester: ~p Metals, ~p Water, ~p Carbon~n", [?HARVESTER_METALS, ?HARVESTER_WATER, ?HARVESTER_CARBON])
			end;
		Type == 'Cargo ship' ->
			Reply = gen_server:call(solar_system, {build, ?CARGO_SHIP_METALS, ?CARGO_SHIP_WATER, ?CARGO_SHIP_CARBON}),
			if
				Reply == build_ok ->
					building(Type);
				true ->
					arbitrator:format("Not enough resources~n", []),
					arbitrator:format("Cargo ship: ~p Metals, ~p Water, ~p Carbon~n", [?CARGO_SHIP_METALS, ?CARGO_SHIP_WATER, ?CARGO_SHIP_CARBON])
			end;
		Type == 'Escort' ->
			Reply = gen_server:call(solar_system, {build, ?ESCORT_METALS, ?ESCORT_WATER, ?ESCORT_CARBON}),
			if
				Reply == build_ok ->
					building(Type);
				true ->
					arbitrator:format("Not enough resources~n", []),
					arbitrator:format("Escort ship: ~p Metals, ~p Water, ~p Carbon~n", [?ESCORT_METALS, ?ESCORT_WATER, ?ESCORT_CARBON])
			end;
		Type == 'Spy drone' ->
			Reply = gen_server:call(solar_system, {build, ?SPY_METALS, ?SPY_WATER, ?SPY_CARBON}),
			if
				Reply == build_ok ->
					building(Type);
				true ->
					arbitrator:format("Not enough resources~n", []),
					arbitrator:format("Spy drone: ~p Metals, ~p Water, ~p Carbon~n", [?SPY_METALS, ?SPY_WATER, ?SPY_CARBON])
			end;
		true ->
			io:format("Unkown Type: ~p~n", [SType]),
			arbitrator:format("Unkown Type: ~p", [SType]),
			false
	end.

%% Building function sleeps for the time it takes to build ship of Type
building(Type) ->
	SType = atom_to_list(Type),
	arbitrator:format("Building: ~p~n", [SType]),
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
			io:format("Error in building function~n")
	end,
	gen_server:cast(solar_system, {building, Type}),
	arbitrator:format("Done building: ~p~n", [SType]).

% Start a harvesting operation on a location of type 'Type'
% If no harvesters are available, nothing happens
harvest(Type) ->
	IsResource = lists:member(Type, ['Metals', 'Water', 'Carbon']),
	if IsResource == true ->
		io:format("Harvest~n"),
		Reply = gen_server:call(solar_system, {start_harvest, Type}),
		io:format("reply: ~p~n", [Reply]),
		if
			Reply == badResource -> 
				arbitrator:format("There is no ~p in this solar system~n", [Type]);
			Reply == ship ->
				spawn(solar_system, harvesting, [Type]);
			true ->
				false
		end;
	true -> arbitrator:format("~p is not a resource ~n", [Type])
	end.

% Perform a harvesting operation of the given type and after waiting for  
% some time, sends the result to the server
harvesting(Type) ->
	io:format("Harvesting~n"),
	randomSleep(?MIN_HARVEST_TIME, ?MAX_HARVEST_TIME),
	gen_server:cast(solar_system, {harvest, Type, random:uniform(?MAX_HARVEST)}).

%% Death Ray activated send to all nodes reset of resources and ships
destroy_everything() ->
	gen_server:cast(solar_system, deathray).

%% Send to all nodes trade request
trade_request(TWant, THave) ->
	IsResource = lists:member(TWant, ['Metals', 'Water', 'Carbon']) and lists:member(THave, ['Metals', 'Water', 'Carbon']),
	if IsResource == true ->
		arbitrator:format("Broadcasting need for ~p, offering ~p~n", [TWant,THave]),
		Fun = fun(N) -> send(rtrade, {TWant, THave}, N) end,
		lists:foreach(Fun, nodes());
	true -> arbitrator:format("Not a valid resource~n", [])
	end.

%% Send to all nodes cancel request	
cancel_request(TWant, THave) ->
	Fun = fun(N) -> send(ctrade, {TWant, THave}, N) end,
	lists:foreach(Fun, nodes()).	

%% Cancel offer
cancel_offer(Node) ->
	gen_server:cast(solar_system, {cOutOffer, Node}),
	send(coffer, {}, Node).

%% Check if offer is possible then send offer to Node
offer(Node, TWant, QT, THave, QH, NumberOfEscorts) ->
	io:format("Offer~n"),
		
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
					arbitrator:format("Offer sent to ~p: ~p ~p for ~p ~p~n", [Node, THave, QH, TWant, QT]),
					send(offer, {TWant, QT, THave, QH}, Node),
					gen_server:cast(solar_system, {Node, outoffer, {TWant, QT, THave, QH, NumberOfEscorts}})
			end;
		true ->
			arbitrator:format("Outstanding offer to ~p present.~n", [Node])
	end.
%% Accept offer from Node if possible.	
accept_offer(Node, NumberOfEscorts) ->
	% First check if resources are available
	io:format("Are resources available?~n"),
	{THave, Qty, _, _} = gen_server:call(solar_system, {get_offer_from, Node}),
	
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
	Reply = gen_server:call(solar_system, reserve_drone),
	if
		Reply == nodrone ->
			arbitrator:format("There are no available spy drones for this mission!~n", []);
		true ->
			randomSleep(?MIN_SPY_TIME, ?MAX_SPY_TIME),
			{Res, Ships} = sendWait(spy, [], Node, 5000),
			randomSleep(?MIN_SPY_TIME, ?MAX_SPY_TIME),
			arbitrator:format("~p resources: ~w~n", [Node, dict:to_list(Res)]),
			arbitrator:format("~p ships: ~w~n", [Node, dict:to_list(Ships)]),
			gen_server:cast(solar_system, return_drone),
			ok
	end.
	
%% Spawns resource planets in to solar system	
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
	% The state consists of 5 dictionaries: 
	% Resources: available resources
	% Ships: available ships
	% TradeRes: resources reserved for active offers
	% Requests: Trade requests from other nodes
	% Offers: Offers from other nodes
	% OutOffers: Offers to other nodes
	% Contacts: Nodes we have made contact with
	Resources = dict:from_list([{'Metals', 10}, {'Water', 10}, {'Carbon', 10}]),
	Ships = dict:from_list([{'Cargo ship', 0}, {'Harvester', 1}, {'Escort', 0}, {'Spy drone', 0}]),
	TradeRes = dict:from_list([{'Metals', 0}, {'Water', 0}, {'Carbon', 0}]),
	Requests = dict:from_list([]),
	Offers = dict:from_list([]),
	OutOffers = dict:from_list([]),
	Contacts = dict:from_list([]),
	DR = false,
	ResourceType = random(0, 1),
	arbitrator:update_ships(dict:to_list(Ships)),
	arbitrator:update_resources(dict:to_list(Resources)),
	if ResourceType == 0 -> 
		arbitrator:format("This solar system has ~p~n", ["Carbon"]),
		{ok, {Resources, Ships, TradeRes, Requests, Offers, OutOffers, Contacts, DR, false}};
	true -> 
		arbitrator:format("This solar system has ~p~n", ["Water"]),
		{ok, {Resources, Ships, TradeRes, Requests, Offers, OutOffers, Contacts, DR, true}}
	end.

%% checks if there are enough resources if so detract from resources
%% and reply with ok to build else reply with don't build
handle_call({build, Metals, Water, Carbon}, _From, State) ->
	{Res, Ships, Trade, Req, Off, Out, Con, DR, System} = State,
	I = dict:fetch('Metals', Res),
	F = dict:fetch('Water', Res),
	G = dict:fetch('Carbon', Res),
	if 
		I >= Metals andalso F >= Water andalso G >= Carbon ->
			TempRes1 = dict:update_counter('Metals', -Metals, Res),
			TempRes2 = dict:update_counter('Water', -Water, TempRes1),

			NewRes = dict:update_counter('Carbon', -Carbon, TempRes2),
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
	if (System == false) and (Type == 'Water') or (System == true) and (Type == 'Carbon') ->
		{reply, badResource, State};
	true ->
		H = dict:fetch('Harvester', Ships),
		if 
			H == 0 -> 
				{reply, noship, State};
			true -> 
				NewShips = dict:update_counter('Harvester', -1, Ships),
				arbitrator:update_ships(dict:to_list(NewShips)),
				{reply, ship, {Res, NewShips, Trade, Req, Off, Out, Con, DR, System}}
		end
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
					NewShips = dict:update_counter('Escort', NumberOfEscorts, TempShips),
					NewTrade = dict:update_counter(Type, Qty, Trade),
					arbitrator:update_resources(dict:to_list(NewRes)),
					arbitrator:update_ships(dict:to_list(NewShips)),
					{reply, ok, {NewRes, NewShips, NewTrade, Req, Off, Out, Con, DR, System}};
				true -> 
					{reply, nores, State}
			end
	end;
handle_call({get_offer_from, Node}, _From, State) ->
	{_, _, _, _, Off, _, _, _, _} = State,
	[Offer] = dict:fetch(Node, Off),
	{reply, Offer, State};
handle_call({have_offer_to, Node}, _From, State) ->
	{_, _, _, _, _, Out, _, _, _} = State,
	Reply = dict:is_key(Node, Out),
	%io:format("Reply: ~p~n", [Reply]),
	{reply, Reply, State};
handle_call({Node, accept_offer, _Msg}, _From, State) ->
	%% Check if the key Node exists in out offers, if so confirm trade, otherwise cancel
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
handle_call(get_number_of_escorts, _From, State) ->
	{_, Ships, _, _, _, _, _, _, _} = State,
	Nescorts = dict:fetch('Escort', Ships),
	{reply, Nescorts, State};
handle_call(get_contacts, _From, State) ->
	{_, _, _, _, _, _, Con, _, _} = State,
	Contacts = dict:fetch_keys(Con),
	{reply, Contacts, State};
handle_call(get_outgoing_offers, _From, State) ->
	{_, _, _, _, _, Out, _, _, _} = State,
	{reply, Out, State};
handle_call(get_incoming_offers, _From, State) ->
	{_, _, _, _, Off, _, _, _, _} = State,
	{reply, Off, State};
handle_call(reserve_drone, _From, State) ->
	io:format("Reserver drone if available~n"),
	{Res, Ships, Trade, Req, Off, Out, Con, DR} = State,
	S = dict:fetch('Spy drone', Ships),
	if 
		S == 0 -> 
			{reply, nodrone, State};
		true -> 
			NewShips = dict:update_counter('Spy drone', -1, Ships),
			arbitrator:update_ships(dict:to_list(NewShips)),
			{reply, ok, {Res, NewShips, Trade, Req, Off, Out, Con, DR}}
	end;
handle_call({_Node, spy, _Msg}, _From, State) ->
	%% Check if the key Node exists in out offers, if so confirm trade, otherwise cancel
	{Res, Ships, _, _, _, _, _, _} = State,
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
	arbitrator:format("!!! Private message from ~w: ~p !!!~n", [Node, Msg]),
	{noreply, {Resources, Ships, Trade, Req, Off, Out, NewCon, DR, System}};
handle_cast({_Node, deathray, {}}, State) ->
	io:format("You have been destroyed by the death ray :(~n"),
	{_, _, _, Req, Off, Out, Con, _, System} = State,
	NewRes = dict:from_list([{'Metals', 0}, {'Water', 0}, {'Carbon', 0}]),
	NewShips = dict:from_list([{'Cargo ship', 0}, {'Harvester', 0}, {'Escort', 0}]),
	NewTradeRes = dict:from_list([{'Metals', 0}, {'Water', 0}, {'Carbon', 0}]),
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
handle_cast({Node, offer, {TWant, QT, THave, QH}}, State) ->
	io:format("Offer from ~w: ~wx~w for ~wx~w~n", [Node, TWant, QT, THave, QH]),
	%io:format("State is: ~p~n", [State]),
	%TODO: Update offer list in GUI.
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR, System} = State,
	Fun = fun(Old) -> Old end,
	NOff = dict:update(Node, Fun, [{TWant, QT, THave, QH}], Off),
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
handle_cast({offer_confirmed, Node, NumberOfEscorts}, State) ->
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR, System} = State,
	[{THad, QH, TGot, QG}] = dict:fetch(Node, Off),
	
	spawn(solar_system, transport, [TGot, QG, NumberOfEscorts]),
	
	%% Update dictionaries
	NewOff = dict:erase(Node, Off), 
	NewTradeRes = dict:update_counter(THad, -QH, TradeRes),

	arbitrator:update_offers(NewOff),
	
	{noreply, {Res, Ships, NewTradeRes, Req, NewOff, Out, Con, DR, System}};
handle_cast({offer_cancelled, Node}, State) ->
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR, System} = State,
	[{THad, QH, _, _}] = dict:fetch(Node, Off),
	
	%% Update dictionaries
	NewOff = dict:erase(Node, Off), 
	NewShips = dict:update_counter('Cargo ship', 1, Ships),
	NewRes = dict:update_counter(THad, QH, Res),
	NewTradeRes = dict:update_counter(THad, -QH, TradeRes),
	
	arbitrator:update_ships(dict:to_list(NewShips)),
	arbitrator:update_resources(dict:to_list(NewRes)),
	arbitrator:update_offers(NewOff),
	
	{noreply, {NewRes, NewShips, NewTradeRes, Req, NewOff, Out, Con, DR, System}};
handle_cast({transport_done, Type, Qt, NumberOfEscorts}, State) ->
	io:format("Gen_server: transport is done ~n This function should update the resources instead: ~p ~p ~n", [Type, Qt]),
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR, System} = State,
	TempShips = dict:update_counter('Cargo ship', 1, Ships),
	NewShips = dict:update_counter('Escort', NumberOfEscorts, TempShips),
	NewRes = dict:update_counter(Type, Qt, Res),
	arbitrator:update_ships(dict:to_list(NewShips)),
	arbitrator:update_resources(dict:to_list(NewRes)),
	{noreply, {NewRes, NewShips, TradeRes, Req, Off, Out, Con, DR, System}};
handle_cast(clear_trade_requests, State) ->
	{Res, Ships, TradeRes, _, Off, Out, Con, DR, System} = State,
	NewReq = dict:from_list([]),
	arbitrator:update_trade_requests(NewReq),
	{noreply, {Res, Ships, TradeRes, NewReq, Off, Out, Con, DR, System}};
handle_cast(deathray, State) ->
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR, System} = State,
	if DR == true ->
		arbitrator:format("Activating Death Ray~n", []),
		Fun = fun(N) -> 
			arbitrator:format("Terminating ~p~n", [N]),
			send(deathray, {}, N) 
		end,
		lists:foreach(Fun, nodes()),
		{noreply, {Res, Ships, TradeRes, Req, Off, Out, Con, false, System}};
	true -> 
		arbitrator:format("You need to build Death Ray first~n", []),
		{noreply, State}
	end;
handle_cast(return_drone, State) ->
	{Res, Ships, TradeRes, Req, Off, Out, Con, DR} = State,
	NewShips = dict:update_counter('Spy drone', 1, Ships),
	arbitrator:update_ships(dict:to_list(NewShips)),
	{noreply, {Res, NewShips, TradeRes, Req, Off, Out, Con, DR}};
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

transport(Type, Qt, NumberOfEscorts) -> 
	arbitrator:format("Currently transporting ~p ~p with ~p escorts ~n", [Qt, Type, NumberOfEscorts]),
	sleep(5000),
	gen_server:cast(solar_system, {transport_done, Type, Qt, NumberOfEscorts}).
