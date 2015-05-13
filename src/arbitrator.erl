-module(arbitrator).
-export([update_resources/1, 
	update_contacts/1, 
	update_ships/1, 
	update_offers/1, 
	receive_message/1, 
	request_trade/2, 
	cancel_trade/2, 
	offer/5, 
	harvest/1,
	build/1]).

update_resources(P) -> client:notify({resources, dic_list_atom_to_string(P)}).
update_contacts(P) -> client:notify({contacts, dic_list_atom_to_string(P)}).
update_ships(P) -> client:notify({ships, dic_list_atom_to_string(P)}).
update_offers(P) -> client:notify({offers, dic_list_atom_to_string(P)}).%need to change
receive_message(M) -> client:notify({message, M}).

%%%% GUI to Solar System
%%% Trade Section
%% Send to all nodes request trade  
request_trade(Want, Have) -> solar_system:trade_request(l2a(Want), l2a(Have)).
%% Send to all nodes cancel trade 
cancel_trade(Want, Have) -> solar_system:cancel_request(l2a(Want), l2a(Have)).
%% Spawns offer process that handle offer
offer(Node, Want, WQ, Have, HQ) -> solar_system:offer(l2a(Node), l2a(Want), l2i(WQ), l2a(Have), l2i(HQ)).

%%% Inside Solar System 
%% Start harvesting mission of type Type
harvest(Type) -> solar_system:harvest(l2a(Type)).
%% build ship of type Type
build(Type) -> solar_system:build(l2a(Type)).
 

%%%% Helper functions
l2a(N) -> list_to_atom(N).
l2i(N) -> list_to_integer(N).

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
