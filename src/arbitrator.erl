-module(arbitrator).
-export([update_resources/1, 
	update_contacts/1, 
	update_ships/1, 
	update_offers/1, 
	receive_message/1, 
	format/2, 
	die/0, 
	connect/1, 
	set_node_name/1, 
	request_trade/2, 
	cancel_trade/2, 
	offer/5, 
	harvest/1,
	build/1,
	resource_types/0,
	ship_types/0,
	send_private_message/2,
	get_contacts/0]).

%%%% Solar System to GUI
update_contacts(D) ->
	Keys = dict:fetch_keys(D),
	Result = requests_to_list(Keys, D),
	client:notify({contacts, Result}).

update_offers(P) ->
	Result = offers_to_list(dict:to_list(P)),
	io:format("OFFER LIST: ~p~n", [Result]),
	client:notify({offers, Result}).
offers_to_list([]) -> [];
offers_to_list([H|T]) ->
	io:format("H is: ~p~n", [H]),
	{Node, [L]} = H,
	{A, B, C, D} = L,
	[{a2l(Node), a2l(A), integer_to_list(B), a2l(C), integer_to_list(D)}] ++ offers_to_list(T).
	
update_resources(P) -> client:notify({resources, dic_list_atom_to_string(P)}).
update_ships(P) -> client:notify({ships, dic_list_atom_to_string(P)}).
%update_offers(P) -> client:notify({offers, dic_list_atom_to_string(P)}).%need to change
receive_message(M) -> client:notify({message, M}).
format(S, P) -> 
	io:format(S,P),
	client:notify({format, S, P}).
die() -> client:notify(die).
built_death_ray() -> ok.


%%%% GUI to Solar System
%% Connect to network of nodes
connect(Node) -> solar_system:connect(l2a(Node)).
%% Sets your node name
set_node_name(Node) -> 	solar_system:set_node_name(l2a(Node)).
%% Send the message Msg to Node 
send_private_message(Node, Msg) -> 
	solar_system:send(msg, Msg, l2a(Node)),
	client:notify({format, "Sent ~p to ~p ~n", [Msg, Node]}).

%%% Trade Section
%% Send to all nodes request trade  
request_trade(Want, Have) -> solar_system:trade_request(l2a(Want), l2a(Have)).
%% Send to all nodes cancel trade 
cancel_trade(Want, Have) -> solar_system:cancel_request(l2a(Want), l2a(Have)).
%% Spawns offer process that handle offer
offer(Node, Want, WQ, Have, HQ) -> solar_system:offer(l2a(Node), l2a(Want), WQ, l2a(Have), HQ).

%%% Inside Solar System 
%% Start harvesting mission of type Type
harvest(Type) -> solar_system:harvest(l2a(Type)).
%% build ship of type Type
build(Type) -> solar_system:build(l2a(Type)).
%% Returns resource types
resource_types() -> solar_system:resource_types().
%% Returns ship types
ship_types() -> solar_system:ship_types(). 

get_contacts() -> 
	atom_list_to_string_list(solar_system:get_contacts()).

%%%% Helper functions
l2a(N) -> list_to_atom(N).
l2i(N) -> list_to_integer(N).
a2l(N) -> atom_to_list(N).

dic_list_atom_to_string(L) ->
	dic_list_atom_to_string(L,[]).

dic_list_atom_to_string([],R) -> R;
dic_list_atom_to_string([H|L],[]) ->
	{A, B} = H,
	H2 = {atom_to_list(A), integer_to_list(B)},
	dic_list_atom_to_string(L, [H2]);	
dic_list_atom_to_string([H|L],R) ->
	{A, B} = H,
	H2 = {atom_to_list(A), integer_to_list(B)},
	dic_list_atom_to_string(L, R ++ [H2] ).
	
requests_to_list([], _) -> [];
requests_to_list([H|T], D) ->
	key_to_list(H, dict:fetch(H, D)) ++ requests_to_list(T, D).
	
key_to_list(_, []) -> [];
key_to_list(Key, [H|T]) ->
	{Want, Have} = H,
	[{a2l(Key), a2l(Want), a2l(Have)}] ++ key_to_list(Key, T).
	
atom_list_to_string_list([]) -> [];
atom_list_to_string_list([H|T]) ->
	[a2l(H)] ++ atom_list_to_string_list(T).
	 
