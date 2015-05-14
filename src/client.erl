-module(client).
-export([start/0, start/1, start_link/0, start_link/1, format/3, 
	 init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2,
	 handle_update/1, notify/1]).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).

-record(state, {win, log, resources, contacts, ships, offers, env}).

-define(ID_DEATH_RAY, 101).
-define(ID_HARVEST, 102).
-define(ID_TRADE, 103).
-define(ID_BROADCAST, 104).
-define(ID_MESSAGE, 105).
-define(ID_CONNECT, 106).
-define(ID_HARVESTER, 107).
-define(ID_CARGO_SHIP, 108).
-define(ID_ESCORT, 109).
-define(ID_IDENTIFY, 110).
-define(ID_OFFERS, 111).

start() ->
	start([]).

start(Debug) ->
	wx_object:start(?MODULE, Debug, []).

start_link() ->
	start_link([]).

start_link(Debug) ->
	wx_object:start_link(?MODULE, Debug, []).

format(Log, Str, Args) ->
	wxTextCtrl:appendText(Log, io_lib:format(Str, Args)),
	ok.

init(Options) ->
	wx:new(Options),
	process_flag(trap_exit, true),

	Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Kardashiev Client"),
	MB = wxMenuBar:new(),
	Build	= wxMenu:new([]),
	wxMenu:append(Build, ?ID_HARVESTER, "&Harvester"),
	wxMenu:append(Build, ?ID_CARGO_SHIP, "&Cargo Ship"),
	wxMenu:append(Build, ?ID_ESCORT, "&Escort"),
	wxMenu:appendSeparator(Build),
	wxMenu:append(Build, ?ID_DEATH_RAY, "&Death Ray"),
	Mission = wxMenu:new([]),
	wxMenu:append(Mission, ?ID_HARVEST, "&Harvest"),
	wxMenu:append(Mission, ?ID_TRADE, "&Trade"),
	Comms	= wxMenu:new([]),
	wxMenu:append(Comms, ?ID_BROADCAST, "&Broadcast"),
	wxMenu:append(Comms, ?ID_OFFERS, "&My Offers"),
	wxMenu:appendSeparator(Comms),
	wxMenu:append(Comms, ?ID_MESSAGE, "&Message"),
	wxMenu:appendSeparator(Comms),
	wxMenu:append(Comms, ?ID_CONNECT, "&Connect"),
	wxMenu:append(Comms, ?ID_IDENTIFY, "&Set Name"),
	Game	= wxMenu:new([]),
	wxMenu:append(Game, ?wxID_ABOUT, "&About"),
	wxMenu:appendSeparator(Game),
	wxMenu:append(Game, ?wxID_EXIT, "&Quit"),

	wxMenuBar:append(MB, Build, "&Build"),
	wxMenuBar:append(MB, Mission, "&Mission"),
	wxMenuBar:append(MB, Comms, "&Communications"),
	wxMenuBar:append(MB, Game, "&Game"),

	wxFrame:setMenuBar(Frame, MB),

	wxFrame:connect(Frame, command_menu_selected),
	wxFrame:connect(Frame, close_window),

	_SB = wxFrame:createStatusBar(Frame, []),

	TopSplitter   = wxSplitterWindow:new(Frame, [{style, ?wxSP_NOBORDER}]),
	MainPanel = wxPanel:new(TopSplitter, []),
	MainSizer = wxBoxSizer:new(?wxHORIZONTAL),

	LSizer = wxBoxSizer:new(?wxVERTICAL),
	RSizer = wxBoxSizer:new(?wxVERTICAL),

	{ResourcePanel, Resources} = create_list_ctrl(MainPanel, [{0, "Resource"}, {1, "Quantity"}]),
	{ShipPanel, Ships} = create_list_ctrl(MainPanel, [{0, "Ship"}, {1, "Quantity"}]),
	{ContactPanel, Contacts} = create_list_ctrl(MainPanel, [{0, "Contact"}, {1, "Have"}, {2, "Want"}]),
	{OfferPanel, Offers} = create_list_ctrl(MainPanel, [{0, "Contact"}, {1, "Qty (request)"}, {2, "Request"}, {3, "Offer"}, {4, "Qty (offer)"}]),

	ResourceSizer = wxStaticBoxSizer:new(?wxVERTICAL, MainPanel, [{label, "Resources"}]),
	ShipSizer = wxStaticBoxSizer:new(?wxVERTICAL, MainPanel, [{label, "Ships"}]),

	ContactSizer = wxStaticBoxSizer:new(?wxVERTICAL, MainPanel, [{label, "Contacts"}]),
	OfferSizer = wxStaticBoxSizer:new(?wxVERTICAL, MainPanel, [{label, "Offers"}]),

	SzOpts = [{border, 8}, {proportion, 0}, {flag, ?wxALL bor ?wxEXPAND }],

	wxSizer:add(ResourceSizer, ResourcePanel, SzOpts),
	wxSizer:add(ShipSizer, ShipPanel, SzOpts),
	wxSizer:add(ContactSizer, ContactPanel, SzOpts),
	wxSizer:add(OfferSizer, OfferPanel, SzOpts),

	wxSizer:add(LSizer, ResourceSizer, SzOpts),
	wxSizer:add(LSizer, ShipSizer, SzOpts),

	wxSizer:add(RSizer, ContactSizer, SzOpts),
	wxSizer:add(RSizer, OfferSizer, SzOpts),

	wxSizer:add(MainSizer, LSizer, SzOpts),
	wxSizer:add(MainSizer, RSizer, SzOpts),

	AddEvent = fun(Parent) ->
			   EventText = wxTextCtrl:new(Parent, 
						  ?wxID_ANY, 
						  [{style, ?wxTE_DONTWRAP bor 
							?wxTE_MULTILINE bor ?wxTE_READONLY}
						  ]),
			   wxTextCtrl:appendText(EventText, "Welcome, Lord Quas.\n"),
			   EventText
		   end,

	{EvPanel, [EvCtrl],_} = create_subwindow(TopSplitter, "Messages", [AddEvent]),
	wxSplitterWindow:splitHorizontally(TopSplitter, MainPanel, EvPanel, [{sashPosition, 600}]),


	State = #state{win=Frame, log=EvCtrl, resources=Resources, contacts=Contacts, ships=Ships, offers=Offers, env=wx:get_env()},
	Watcher = whereis(refresher),
	if
		Watcher =/= undefined ->
			Watcher ! die,
			receive
				_ -> true
			after 2000 -> true
			end;
		true -> true
	end,
	Pid = spawn(client, handle_update, [State]),
	register(refresher, Pid),

	wxPanel:setSizer(MainPanel, MainSizer),
	wxFrame:show(Frame),

	{Frame, State}.

%% Helpers

-define(ID_OFFER_WIN, 200).
-define(ID_OFFER_R, 201).
-define(ID_OFFER_O, 202).
-define(ID_OFFER_RQ, 203).
-define(ID_OFFER_OQ, 204).

make_offer(State) ->
	Frame = wxFrame:new(State#state.win, ?ID_OFFER_WIN, "Make Trade Offer",
						[{style,
							?wxCAPTION bor
							?wxCLOSE_BOX bor
							?wxSTAY_ON_TOP
						}]),

	Avail = arbitrator:resource_types(),
	Owned = arbitrator:resource_types(),

	Panel = wxPanel:new(Frame, []),
	SuperSizer = wxBoxSizer:new(?wxVERTICAL),
	MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
	RequestSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel,
										[{label, "Request"}]),
	OfferSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel,
										[{label, "Offer"}]),

	ReqSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel),
	OffSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel),

	ReqPanel1 = wxPanel:new(Panel, []),
	OffPanel1 = wxPanel:new(Panel, []),
	ReqPanel2 = wxPanel:new(Panel, []),
	OffPanel2 = wxPanel:new(Panel, []),

	_Request = wxChoice:new(ReqPanel1, ?ID_OFFER_R, [{choices, Avail}]),
	_Offer = wxChoice:new(OffPanel1, ?ID_OFFER_O, [{choices, Owned}]),

	RQ = wxSpinCtrl:new(ReqPanel2, [{id, ?ID_OFFER_RQ}]),
	OQ = wxSpinCtrl:new(OffPanel2, [{id, ?ID_OFFER_OQ}]),

	wxSpinCtrl:setRange(RQ, 1, 10),
	wxSpinCtrl:setRange(OQ, 1, 10),

	CancelPanel = wxPanel:new(Panel, []),
	ConfirmPanel = wxPanel:new(Panel, []),
	ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),

	_Cancel = wxButton:new(CancelPanel, ?wxID_DELETE, [{label, "Cancel"}]),
	_Confirm = wxButton:new(ConfirmPanel, ?wxID_APPLY, [{label, "Confirm"}]),

	wxWindow:connect(Panel, command_button_clicked),

	Options = [{border, 8}, {proportion, 0}, {flag, ?wxALL bor ?wxEXPAND}],
	wxSizer:add(ButtonSizer, CancelPanel, Options),
	wxSizer:add(ButtonSizer, ConfirmPanel, Options),

	wxSizer:add(ReqSizer, ReqPanel1, Options),
	wxSizer:add(ReqSizer, ReqPanel2, Options),
	wxSizer:add(OffSizer, OffPanel1, Options),
	wxSizer:add(OffSizer, OffPanel2, Options),

	wxSizer:add(RequestSizer, ReqSizer, Options),
	wxSizer:add(OfferSizer, OffSizer, Options),
	wxSizer:add(MainSizer, RequestSizer, Options),
	wxSizer:add(MainSizer, OfferSizer, Options),
	wxSizer:add(SuperSizer, MainSizer, Options),
	wxSizer:add(SuperSizer, ButtonSizer, Options),

	wxPanel:setSizer(Panel, SuperSizer),
	wxFrame:center(Frame),
	wxFrame:show(Frame).

create_subwindow(Parent, BoxLabel, Funs) ->
	Panel = wxPanel:new(Parent),
	Sz	  = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, BoxLabel}]),
	wxPanel:setSizer(Panel, Sz),
	Ctrls = [Fun(Panel) || Fun <- Funs],
	[wxSizer:add(Sz, Ctrl, [{proportion, 1}, {flag, ?wxEXPAND}]) 
	 || Ctrl <- Ctrls],
	{Panel, Ctrls, Sz}.

cols_to_listctrl(_, []) -> ok;
cols_to_listctrl(Ctrl, [H|T]) ->
	{N, S} = H,
	wxListCtrl:insertColumn(Ctrl, N, S),
	cols_to_listctrl(Ctrl, T).

create_list_ctrl(Parent, L) ->
	Panel = wxPanel:new(Parent, []),
	ListCtrl = wxListCtrl:new(Panel, [{style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}, {size, {500, 300}}]),
	cols_to_listctrl(ListCtrl, L),
	{Panel, ListCtrl}.

add_message(State, M) ->
	S = M ++ "~n",
	format(State#state.log, S, []).

update_offers(State, Offers) ->
	ListCtrl = State#state.offers,
	wxListCtrl:deleteAllItems(ListCtrl),
	insert_offer(ListCtrl, Offers),
	ListCtrl.

update_ships(State, Ships) ->
	ListCtrl = State#state.ships,
	io:format("State is: ~p~n", [ListCtrl]),
	wxListCtrl:deleteAllItems(ListCtrl),
	insert_resource(ListCtrl, Ships), % not a typo!! code re-use
	ListCtrl.

update_contacts(State, Contacts) ->
	ListCtrl = State#state.contacts,
	wxListCtrl:deleteAllItems(ListCtrl),
	insert_contact(ListCtrl, Contacts),
	ListCtrl.

update_resources(State, Resources) ->
	ListCtrl = State#state.resources,
	wxListCtrl:deleteAllItems(ListCtrl),
	insert_resource(ListCtrl, Resources),
	ListCtrl.

insert_offer(Ctrl, []) -> Ctrl;
insert_offer(Ctrl, [C|T]) ->
	{N, Q1, H, W, Q2} = C,
	wxListCtrl:insertItem(Ctrl, 0, ""),
	wxListCtrl:setItem(Ctrl, 0, 0, N),
	wxListCtrl:setItem(Ctrl, 0, 1, Q1),
	wxListCtrl:setItem(Ctrl, 0, 2, H),
	wxListCtrl:setItem(Ctrl, 0, 3, W),
	wxListCtrl:setItem(Ctrl, 0, 4, Q2),
	insert_offer(Ctrl, T).

insert_contact(Ctrl, []) -> Ctrl;
insert_contact(Ctrl, [C|T]) ->
	{N, H, W} = C,
	wxListCtrl:insertItem(Ctrl, 0, ""),
	wxListCtrl:setItem(Ctrl, 0, 0, N),
	wxListCtrl:setItem(Ctrl, 0, 1, H),
	wxListCtrl:setItem(Ctrl, 0, 2, W),
	insert_contact(Ctrl, T).

insert_resource(Ctrl, []) -> Ctrl;
insert_resource(Ctrl, [R|T]) ->
	{N, Q} = R,
	I = 0,
	wxListCtrl:insertItem(Ctrl, I, ""),
	wxListCtrl:setItem(Ctrl, I, 0, N),
	wxListCtrl:setItem(Ctrl, I, 1, Q),
	insert_resource(Ctrl, T).

send_message(State, Recipient) ->
	Frame = State#state.win,
	Str = "Message body:",
	Caption = io_lib:format("Send a message to ~p:", [Recipient]),
	Dialog = wxTextEntryDialog:new(Frame,
								   Str,
								   [{style, ?wxOK bor ?wxCANCEL},
									{caption, Caption},
									{value, "Hello."}]),
	Ret = wxDialog:showModal(Dialog),
	if Ret == ?wxID_OK ->
		Msg = wxTextEntryDialog:getValue(Dialog),
		arbitrator:send_private_message(Recipient, Msg);
	true -> true
	end,
	wxDialog:destroy(Dialog),
	ok.

connect_d(State) ->
	Frame = State#state.win,
	Str = "Connect to:",
	Dialog = wxTextEntryDialog:new(Frame,
								   Str,
								   [{style, ?wxOK bor ?wxCANCEL},
									{caption, "Connect to node"},
									{value, "name@ip_addr"}]),
	Ret = wxDialog:showModal(Dialog),
	if Ret == ?wxID_OK ->
		arbitrator:connect(wxTextEntryDialog:getValue(Dialog));
	true -> true
	end,
	wxDialog:destroy(Dialog),
	ok.

identify_d(State) ->
	Frame = State#state.win,
	Str = "Set name to:",
	Dialog = wxTextEntryDialog:new(Frame,
								   Str,
								   [{style, ?wxOK bor ?wxCANCEL},
									{caption, "Set name"},
									{value, "badname@ip_addr"}]),
	Ret = wxDialog:showModal(Dialog),
	if Ret == ?wxID_OK ->
		Val = wxTextEntryDialog:getValue(Dialog),
		arbitrator:set_node_name(Val);
	true -> Val = none 
	end,
	wxDialog:destroy(Dialog),
	Val.

broadcast_d(State) -> 
	Frame = State#state.win,
	Resources = arbitrator:resource_types(),
	Dialog = wxSingleChoiceDialog:new(Frame,
									  "Requested resource:",
									  "Broadcast demand",
									  Resources),
	D2     = wxSingleChoiceDialog:new(Frame,
									  "Offered resource:",
									  "Broadcast supply",
									  Resources),
	Ret = wxDialog:showModal(Dialog),
	if Ret == ?wxID_OK ->
		Want = wxSingleChoiceDialog:getStringSelection(Dialog),
		R2 = wxDialog:showModal(D2),
		if R2 == ?wxID_OK ->
			   Have = wxSingleChoiceDialog:getStringSelection(D2);
		true -> Have = none
		end,
		wxDialog:destroy(D2);
	true -> Want = none, Have = none
	end,
	wxDialog:destroy(Dialog),
	{Want, Have}.

dialog_harvest_rsrc(State) -> 
	Frame = State#state.win,
	Resources = arbitrator:resource_types(),
	Dialog = wxSingleChoiceDialog:new(Frame,
									  "Harvest resource:",
									  "Harvest",
									  Resources),
	Ret = wxDialog:showModal(Dialog),
	if Ret == ?wxID_OK ->
		Choice = wxSingleChoiceDialog:getStringSelection(Dialog);
	true -> Choice = none
	end,
	wxDialog:destroy(Dialog),
	format(State#state.log, "~p ~n", [Choice]),
	Choice.

node_d(State, Str) -> 
	Frame = State#state.win,
	Nodes = arbitrator:get_contacts(),
	Dialog = wxSingleChoiceDialog:new(Frame,
									  Str,
									  "Contacts",
									  Nodes),
	Ret = wxDialog:showModal(Dialog),
	if Ret == ?wxID_OK ->
		Choice = wxSingleChoiceDialog:getStringSelection(Dialog);
	true -> Choice = none
	end,
	wxDialog:destroy(Dialog),
	Choice.

%% Callbacks
handle_info({'EXIT',_, wx_deleted}, State) ->
	{noreply,State};
handle_info({'EXIT',_, shutdown}, State) ->
	{noreply,State};
handle_info({'EXIT',_, normal}, State) ->
	{noreply,State};
handle_info(Msg, State) ->
	io:format("Got Info ~p~n",[Msg]),
	{noreply,State}.

handle_call(Msg, _From, State) ->
	io:format("Got Call ~p~n",[Msg]),
	{reply,ok,State}.

handle_cast(Msg, State) ->
	io:format("Got cast ~p~n",[Msg]),
	{noreply,State}.

%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{id = Id,
		event = #wxCommand{type = command_button_clicked}},
		State = #state{}) ->
	case Id of
	?wxID_APPLY ->
		W0 = wxWindow:findWindowById(?ID_OFFER_WIN),
		RR = wxWindow:findWindowById(?ID_OFFER_R),
		OR = wxWindow:findWindowById(?ID_OFFER_O),
		RQ = wxWindow:findWindowById(?ID_OFFER_RQ),
		OQ = wxWindow:findWindowById(?ID_OFFER_OQ),
		RC = wx:typeCast(RR, wxChoice),
		OC = wx:typeCast(OR, wxChoice),
		RQC = wx:typeCast(RQ, wxSpinCtrl),
		OQC = wx:typeCast(OQ, wxSpinCtrl),
		W = wxControlWithItems:getStringSelection(RC),
		H = wxControlWithItems:getStringSelection(OC),
		Q1 = wxSpinCtrl:getValue(RQC),
		Q2 = wxSpinCtrl:getValue(OQC),
		wxWindow:destroy(W0),
		N = node_d(State, "Make offer:"),
		if N =/= none, W =/= "", H =/= "" ->
			arbitrator:offer(N, W, Q1, H, Q2);
		true -> true
		end,
		{noreply, State};
	?wxID_DELETE ->
		W0 = wxWindow:findWindowById(?ID_OFFER_WIN),
		wxWindow:destroy(W0),
		{noreply, State};
	_ ->
		format(State#state.log, "Unhandled button press: #~p ~n", [Id]),
		{noreply, State}
	end;
handle_event(#wx{id = Id,
		 event = #wxCommand{type = command_menu_selected}},
		 State = #state{}) ->
	case Id of
	?wxID_ABOUT ->
	AboutString = string:join(["Kardashiev Space Trading & Strategy Game.\n",
					   "By:\n Björn Ingi Baldvinsson,\n",
					   " Jón Reginbald,\n",
					   " Stefanía Bergljót Stefánsdóttir and\n",
					   " Þröstur Thorarensen.\n",
					   "Implemented in Erlang as a part of\n",
					   "CADP-2015 @ Reykjavik University\n",
					   "running under ",
					   wx_misc:getOsDescription(),
					   "."], 
		""),
		wxMessageDialog:showModal(wxMessageDialog:new(State#state.win, AboutString,
							  [{style,
								?wxOK bor
								?wxICON_INFORMATION bor
								?wxSTAY_ON_TOP},
							   {caption, "About"}])),
		{noreply, State};
	?wxID_EXIT ->
		{stop, normal, State};
	?ID_CONNECT ->
		connect_d(State),
		{noreply, State};
	?ID_BROADCAST -> 
		{W, H} = broadcast_d(State),
		if W =/= none, H =/= none ->
			arbitrator:request_trade(W,H),
			format(State#state.log, "Broadcasting need for ~p, offering ~p~n", [W,H]);
		true -> true
		end,
		{noreply, State};
	?ID_MESSAGE ->
		N = node_d(State, "Send to:"),
		send_message(State,N),
		{noreply, State};
	?ID_TRADE ->
		make_offer(State),
		{noreply, State};
	?ID_IDENTIFY ->
		Val = identify_d(State),
		if Val =/= none -> 
			format(State#state.log, "You are now known as: ~p ~n", [Val]);
		true -> true
		end,
		{noreply, State};
	?ID_HARVESTER ->
		arbitrator:build("Harvester"),
		{noreply, State};
	?ID_CARGO_SHIP ->
		arbitrator:build("Cargo ship"),
		{noreply, State};
	?ID_ESCORT ->
		arbitrator:build("Escort"),
		{noreply, State};
	?ID_DEATH_RAY ->
		arbitrator:build("Death Ray"),
		{noreply, State};
	?ID_HARVEST ->
		Resource = dialog_harvest_rsrc(State),
		if Resource =/= none ->
			arbitrator:harvest(Resource);
		true -> true
		end,
		{noreply, State};
	_ ->
		format(State#state.log, "Unhandled event: #~p ~n", [Id]),
		{noreply, State}
	end;
%handle_event(#wx{obj = Panel,
%		event = #wxMouse{type = right_up}},
%		State = #state{menu = Menu}) ->
	%% Open the popup menu
%	wxWindow:popupMenu(Panel, Menu),
%	{noreply, State};
handle_event(#wx{event=#wxClose{}}, State = #state{win=Frame}) ->
	io:format("~p Closing window ~n",[self()]),
	ok = wxFrame:setStatusText(Frame, "Closing...",[]),
	{stop, normal, State};
handle_event(Ev,State) ->
	io:format("~p Got event ~p ~n",[?MODULE, Ev]),
	{noreply, State}.

code_change(_, _, State) ->
	{stop, not_yet_implemented, State}.

terminate(_Reason, State = #state{win=Frame}) ->
	catch wx_object:call(State#state.log, shutdown),
	wxFrame:destroy(Frame),
	wx:destroy().

handle_update(State) ->
	Env = State#state.env,
	wx:set_env(Env),
	receive
		{resources, L} ->
			update_resources(State, L);
		{contacts, L} ->
			update_contacts(State, L);
		{ships, L} ->
			update_ships(State, L);
		{offers, L} ->
			update_offers(State, L);
		{message, M} ->
			add_message(State, M);
		{format, S, P} ->
			format(State#state.log, S, P);
		die ->
			exit(ok)
	end,
	handle_update(State).

notify(L) ->
	refresher ! L.

