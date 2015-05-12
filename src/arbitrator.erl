-module(arbitrator).
-export([update_resources/1, update_contacts/1, update_ships/1, update_offers/1]).

update_resources(P) -> client:notify({resources, P}).
update_contacts(P) -> client:notify({contacts, P}).
update_ships(P) -> client:notify({ships, P}).
update_offers(P) -> client:notify({offers, P}).
