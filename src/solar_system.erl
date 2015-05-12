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
-define(MAX_HARVEST_TIME, 4000).
-define(MIN_HARVEST_TIME, 2000).

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

% returns the ships formatted for the arbitrator
%list_resources(State) ->
%	{_, Ships, _} = State,
%	[{"Cargo ship", fetch(cargo_ship, Ships)}, {"Harvester", fetch(harvester, Ships)}, {"Escort", fetch(escort, Ships)}].
	
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
	
	%The state consists of 3 dictionaries: Resources, Ships and TradeRes.
	Resources = dict:from_list([{'Iron', 0}, {'Food', 0}, {'Gas', 0}]),
	Ships = dict:from_list([{'Cargo ship', 3}, {'Harvester', 3}, {'Escort', 3}]),
	TradeRes = dict:from_list([{'Iron', 0}, {'Food', 0}, {'Gas', 0}]),
	{ok, {Resources, Ships, TradeRes}}.
	
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
			{reply, ship, {Res, NewShips, Trade}}
	end;

%% checks if the resources and ships needed for the given trade is available
handle_call({trade_available, THave, QH}, _From, State) ->
	io:format("Check if enough resources~n"),
	{Res, Ships, Trade} = State,
	C = dict:fetch('Cargo ship', Ships),
	if 
		C == 0 -> 
			{reply, [noship], State};
		true -> 
			R = dict:fetch(THave, Res),
			if 
				R >= QH ->
					NewRes = dict:update_counter(THave, -QH, Res),
					NewShips = dict:update_counter('Cargo ship', -1, Ships),
					NewTrade = dict:update_counter(THave, QH, Trade),
					{reply, [ok], {NewRes, NewShips, NewTrade}};
				true -> 
					{reply, [nores], State}
			end
	end;	
handle_call(_Msg, _From, State) ->
	{reply, [], State}.

%% ends the harvest and increases our current resources accordingly
handle_cast({harvest, Type, Qty}, State) ->
	io:format("harvest cast~n"),
	{Resources, Ships, Trade} = State,
	NewShips = dict:update_counter('Harvester', 1, Ships),
	NewRes = dict:update_counter(Type, Qty, Resources),
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

