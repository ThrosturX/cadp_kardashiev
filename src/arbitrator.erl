-module(arbitrator).
-export([update_resources/1, 
	update_contacts/1, 
	update_ships/1, 
	update_offers/1, 
	receive_message/1, 
	request_trade/2, 
	cancel_trade/2, 
	offer/5, 
	harvest/1]).

update_resources(P) -> client:notify({resources, dic_list_atom_to_string(P)}).
update_contacts(P) -> client:notify({contacts, dic_list_atom_to_string(P)}).
update_ships(P) -> client:notify({ships, dic_list_atom_to_string(P)}).
update_offers(P) -> client:notify({offers, dic_list_atom_to_string(P)}).%need to change
receive_message(M) -> client:notify({message, M}).

%%%% GUI to Solar System
%%% Trade Section
%% Send to all nodes request trade  
request_trade(Want, Have) -> solar_system:trade_request(Want, Have).
%% Send to all nodes cancel trade 
cancel_trade(Want, Have) -> solar_system:cancel_request(Want, Have).
%% Spawns offer process that handle offer
offer(Node, Want, WQ, Have, HQ) -> solar_system:offer(Node, Want, WQ, Have, HQ).

%%% Inside Solar System 
%% Start harvesting mission of type Type
harvest(Type) -> solar_system:harvest(Type).

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
