-module(client).
-export([start/0, start/1, start_link/0, start_link/1, format/3, 
	 init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2,
	 handle_update/1, notify/1]).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).

% record to store "global variables"
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
-define(ID_CANCEL_OFFER, 111).
-define(ID_SEND_OFFER, 112).
-define(ID_ACTIVATE_RAY, 113).
-define(ID_SPY, 114).
-define(ID_CLEAR_REQUESTS, 115).
-define(ID_MENUBAR, 116).
-define(ID_HARVEST_METALS, 117).
-define(ID_HARVEST_RARE, 118).
-define(ID_BUILD_SPY, 119).
-define(ID_BUILD_HARVESTER, 120).
-define(ID_BUILD_CARGO_SHIP, 121).

-define(ID_OFFER_WIN, 200).
-define(ID_OFFER_R, 201).
-define(ID_OFFER_O, 202).
-define(ID_OFFER_RQ, 203).
-define(ID_OFFER_OQ, 204).
-define(ID_ESCORT_SPIN, 205).
-define(ID_ESCORT_SPIN2, 206).

-define(ID_RETRACT, 210).
-define(ID_CLOSE, 211).
-define(ID_MY_OFFERS, 212).

-define(ID_ACCEPT_TRADE, 220).
-define(ID_CLOSE_TRADE, 221).
-define(ID_ACCEPT_OFFER, 222).

% wx boilerplate code...
start() ->
	start([]).

start(Debug) ->
	wx_object:start(?MODULE, Debug, []).

start_link() ->
	start_link([]).

start_link(Debug) ->
	wx_object:start_link(?MODULE, Debug, []).

% print a format string message into the log/messages window
format(Log, Str, Args) ->
	wxTextCtrl:appendText(Log, io_lib:format(Str, Args)),
	ok.

% start wx and display the main window
init(Options) ->
	wx:new(Options),
	process_flag(trap_exit, true),

	% Frame is the main window
	Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Kardashiev Client", [{size, {1000, 700}}]),
	% set up the menu bar and menus
	MB = wxMenuBar:new(),
	Build	= wxMenu:new([]),
	wxMenu:append(Build, ?ID_HARVESTER, "&Harvester"),
	wxMenu:append(Build, ?ID_CARGO_SHIP, "&Cargo Ship"),
	wxMenu:append(Build, ?ID_ESCORT, "&Escort"),
	wxMenu:append(Build, ?ID_BUILD_SPY, "&Spy Drone"),
	wxMenu:appendSeparator(Build),
	wxMenu:append(Build, ?ID_DEATH_RAY, "&Death Ray"),
	Mission = wxMenu:new([]),
	wxMenu:append(Mission, ?ID_HARVEST, "&Harvest..."),
	wxMenu:append(Mission, ?ID_TRADE, "&Trade..."),
	wxMenu:append(Mission, ?ID_SPY, "Send &Spy..."),
	wxMenu:append(Mission, ?ID_ACTIVATE_RAY, "Activate &Death Ray..."),
	Comms	= wxMenu:new([]),
	wxMenu:append(Comms, ?ID_BROADCAST, "&Broadcast..."),
	wxMenu:append(Comms, ?ID_CLEAR_REQUESTS, "Dismiss &Demands"),
	wxMenu:appendSeparator(Comms),
	wxMenu:append(Comms, ?ID_SEND_OFFER, "Send &Offer..."),
	wxMenu:append(Comms, ?ID_CANCEL_OFFER, "&Cancel Offers..."),
	wxMenu:appendSeparator(Comms),
	wxMenu:append(Comms, ?ID_MESSAGE, "&Message..."),
	wxMenu:appendSeparator(Comms),
	wxMenu:append(Comms, ?ID_CONNECT, "Co&nnect..."),
	wxMenu:append(Comms, ?ID_IDENTIFY, "Se&t Name..."),
	Game	= wxMenu:new([]),
	wxMenu:append(Game, ?wxID_ABOUT, "&About"),
	wxMenu:appendSeparator(Game),
	wxMenu:append(Game, ?wxID_EXIT, "&Quit"),

	% attach menus to the menu bar
	wxMenuBar:append(MB, Build, "&Build"),
	wxMenuBar:append(MB, Mission, "&Mission"),
	wxMenuBar:append(MB, Comms, "&Communications"),
	wxMenuBar:append(MB, Game, "&Game"),

	% attach menubar to the main window
	wxFrame:setMenuBar(Frame, MB),

	% connect menu bar and close window events
	wxFrame:connect(Frame, command_menu_selected),
	wxFrame:connect(Frame, close_window),

	% not currently in use, but can be used to display a status bar at the bottom
	_SB = wxFrame:createStatusBar(Frame, []),

	% splitters & sizers are used to format the layout
	% Split the window into two halves, top and bottom, tables at top and message log at bottom
	TopSplitter   = wxSplitterWindow:new(Frame, [{style, ?wxSP_NOBORDER}]),
	MainPanel = wxPanel:new(TopSplitter, []),
	MainSizer = wxBoxSizer:new(?wxVERTICAL),

	LSizer = wxBoxSizer:new(?wxVERTICAL),
	RSizer = wxBoxSizer:new(?wxVERTICAL),

	% create list controls and put them into panels for the sizers
	{ResourcePanel, Resources} = create_list_ctrl(MainPanel, [{0, "Resource"}, {1, "Quantity"}], 200, 100),
	{ShipPanel, Ships} = create_list_ctrl(MainPanel, [{0, "Ship"}, {1, "Quantity"}], 200, 150),
	{ContactPanel, Contacts} = create_list_ctrl(MainPanel, [{0, "Contact"}, {1, "Have"}, {2, "Want"}], 400, 100),
	{OfferPanel, Offers} = create_list_ctrl(MainPanel, [{0, "Contact"}, {1, "Qty (request)"}, {2, "Request"}, {3, "Offer"}, {4, "Qty (offer)"}], 400, 150),

	% create labelled sizers to display informative names of the lists inside them
	ResourceSizer = wxStaticBoxSizer:new(?wxVERTICAL, MainPanel, [{label, "Resources"}]),
	ShipSizer = wxStaticBoxSizer:new(?wxVERTICAL, MainPanel, [{label, "Ships and Agents"}]),

	ContactSizer = wxStaticBoxSizer:new(?wxVERTICAL, MainPanel, [{label, "Demands"}]),
	OfferSizer = wxStaticBoxSizer:new(?wxVERTICAL, MainPanel, [{label, "Offers"}]),
	
	% MainPanel will hold ListSizer and ButtonSizer
	ListSizer = wxBoxSizer:new(?wxHORIZONTAL),
	ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
	HarvestSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, MainPanel, [{label, "Harvest..."}]),
	BuildSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, MainPanel, [{label, "Build..."}]),

	HarvestButtonPanelM = wxPanel:new(MainPanel, []),
	HarvestButtonPanelR = wxPanel:new(MainPanel, []),
	BuildButtonPanelH = wxPanel:new(MainPanel, []),
	BuildButtonPanelC = wxPanel:new(MainPanel, []),

	wxButton:new(HarvestButtonPanelM, ?ID_HARVEST_METALS, [{label, "Metals"}]),
	wxButton:new(HarvestButtonPanelR, ?ID_HARVEST_RARE, [{label, "Precious Materials"}]),

	wxButton:new(BuildButtonPanelH, ?ID_BUILD_HARVESTER, [{label, "Harvester"}]),
	wxButton:new(BuildButtonPanelC, ?ID_BUILD_CARGO_SHIP, [{label, "Cargo Ship"}]),

	wxWindow:connect(MainPanel, command_button_clicked),

	% Connect the sizers together
	SzOpts = [{border, 8}, {proportion, 0}, {flag, ?wxALL bor ?wxEXPAND }],

	wxSizer:add(ResourceSizer, ResourcePanel, SzOpts),
	wxSizer:add(ShipSizer, ShipPanel, SzOpts),
	wxSizer:add(ContactSizer, ContactPanel, SzOpts),
	wxSizer:add(OfferSizer, OfferPanel, SzOpts),

	wxSizer:add(LSizer, ResourceSizer, SzOpts),
	wxSizer:add(LSizer, ShipSizer, SzOpts),

	wxSizer:add(RSizer, ContactSizer, SzOpts),
	wxSizer:add(RSizer, OfferSizer, SzOpts),

	wxSizer:add(ListSizer, LSizer, SzOpts),
	wxSizer:add(ListSizer, RSizer, SzOpts),

	wxSizer:add(HarvestSizer, HarvestButtonPanelM, SzOpts),
	wxSizer:add(HarvestSizer, HarvestButtonPanelR, SzOpts),

	wxSizer:add(BuildSizer, BuildButtonPanelH, SzOpts),
	wxSizer:add(BuildSizer, BuildButtonPanelC, SzOpts),

	wxSizer:add(ButtonSizer, HarvestSizer, SzOpts),
	wxSizer:add(ButtonSizer, BuildSizer, SzOpts),

	wxSizer:add(MainSizer, ListSizer, SzOpts),
	wxSizer:add(MainSizer, ButtonSizer, SzOpts),

	% initialize the message log with a welcome message
	AddEvent = fun(Parent) ->
			   EventText = wxTextCtrl:new(Parent, 
						  ?wxID_ANY, 
						  [{style, ?wxTE_DONTWRAP bor 
							?wxTE_MULTILINE bor ?wxTE_READONLY}
						  ]),
			   % We assume the user is called Lord Quas, but hypothetically this can of course be changed later and have a user-defined or random name
			   wxTextCtrl:appendText(EventText, "Welcome, Lord Quas.\n"),
			   EventText
		   end,

	% create the message log window
	{EvPanel, [EvCtrl],_} = create_subwindow(TopSplitter, "Messages", [AddEvent]),
	% split the elements of TopSplitter and keep the message log smaller
	wxSplitterWindow:splitHorizontally(TopSplitter, MainPanel, EvPanel, [{sashPosition, 520}]),
	wxSplitterWindow:setSashGravity(TopSplitter, 0.80),

	% initialize the State record
	State = #state{win=Frame, log=EvCtrl, resources=Resources, contacts=Contacts, ships=Ships, offers=Offers, env=wx:get_env()},

	% initialize the refresher, killing the old one if necessary
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

	% attach the sizer settings to our main panel and display the main window
	wxPanel:setSizer(MainPanel, MainSizer),
	wxFrame:show(Frame),

	{Frame, State}.

%% Helpers

% Trade mission functionality
accept_offer(State) -> 
	Frame = wxFrame:new(State#state.win, ?ID_TRADE, "Send cargo ship", 
						[{style, 
						?wxCAPTION bor
						?wxCLOSE_BOX bor
						?wxSTAY_ON_TOP},
						{size, {700,400}}]),
	% list available offers
	Offers = arbitrator:get_incoming_offers(),
	
	% initialize layout
	Panel = wxPanel:new(Frame, []),
	Sizer = wxBoxSizer:new(?wxHORIZONTAL),
	
	OSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Incoming Offers"}]),
	{OfferPanel, OfferList} = create_list_ctrl(Panel, [{0, "Contact"}, {1, "Qty (request)"}, {2, "Request"}, {3, "Offer"}, {4, "Qty (offer)"}], 400, 150),
	wxWindow:setId(OfferList, ?ID_ACCEPT_OFFER), % <----------
	insert_offer(OfferList, Offers),

	EscortSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Escorts"}]),
	SpinPanel = wxPanel:new(Panel, []),
	Escorts = wxSpinCtrl:new(SpinPanel, [{id, ?ID_ESCORT_SPIN}]),
	wxSpinCtrl:setRange(Escorts, 0, arbitrator:get_escorts()),
	
	ButtonPanel1 = wxPanel:new(Panel, []),
	ButtonPanel2 = wxPanel:new(Panel, []),

	BSizer = wxBoxSizer:new(?wxVERTICAL),
	wxButton:new(ButtonPanel1, ?ID_ACCEPT_TRADE, [{label, "Accept Offer"}]),
	wxButton:new(ButtonPanel2, ?ID_CLOSE_TRADE, [{label, "Close"}]),
	
	% connect event handlers for these buttons
	wxWindow:connect(Panel, command_button_clicked),
	
	% connect sizers
	Options = [{border, 8}, {proportion, 0}, {flag, ?wxALL bor ?wxEXPAND}],

	wxSizer:add(EscortSizer, SpinPanel, Options),
	wxSizer:add(OSizer, OfferPanel, Options),
	wxSizer:add(OSizer, EscortSizer, Options),
	wxSizer:add(BSizer, ButtonPanel1, Options),
	wxSizer:add(BSizer, ButtonPanel2, Options),
	wxSizer:add(Sizer, OSizer, Options),
	wxSizer:add(Sizer, BSizer, Options),

	% finalize and show
	wxPanel:setSizer(Panel, Sizer),
	wxFrame:center(Frame),
	wxFrame:show(Frame). 

% the next few methods are pretty much the same as accept_offer
% cancel an outgoing offer
cancel_offer(State) ->
	Frame = wxFrame:new(State#state.win, ?ID_CANCEL_OFFER, "Cancel Offer",
						[{style,
						  ?wxCAPTION bor
						  ?wxCLOSE_BOX bor
						  ?wxSTAY_ON_TOP
						 },
						 {size, {700, 400}
						 }]),

	Offers = arbitrator:get_outgoing_offers(),

	Panel = wxPanel:new(Frame, []),
	Sizer = wxBoxSizer:new(?wxHORIZONTAL),
	
	OSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Outgoing Offers"}]),
	{OfferPanel, OfferList} = create_list_ctrl(Panel, [{0, "Contact"}, {1, "Qty (request)"}, {2, "Request"}, {3, "Offer"}, {4, "Qty (offer)"}], 400, 150),
	wxWindow:setId(OfferList, ?ID_MY_OFFERS),
	insert_offer(OfferList, Offers),

	ButtonPanel1 = wxPanel:new(Panel, []),
	ButtonPanel2 = wxPanel:new(Panel, []),

	BSizer = wxBoxSizer:new(?wxVERTICAL),
	wxButton:new(ButtonPanel1, ?ID_RETRACT, [{label, "Retract Offer"}]),
	wxButton:new(ButtonPanel2, ?ID_CLOSE, [{label, "Close"}]),

	wxWindow:connect(Panel, command_button_clicked),

	Options = [{border, 8}, {proportion, 0}, {flag, ?wxALL bor ?wxEXPAND}],

	wxSizer:add(OSizer, OfferPanel, Options),
	wxSizer:add(BSizer, ButtonPanel1, Options),
	wxSizer:add(BSizer, ButtonPanel2, Options),
	wxSizer:add(Sizer, OSizer, Options),
	wxSizer:add(Sizer, BSizer, Options),

	wxPanel:setSizer(Panel, Sizer),
	wxFrame:center(Frame),
	wxFrame:show(Frame).

% send an outgoing offer
make_offer(State) ->
	Frame = wxFrame:new(State#state.win, ?ID_OFFER_WIN, "Make Trade Offer",
						[{style,
							?wxCAPTION bor
							?wxCLOSE_BOX bor
							?wxSTAY_ON_TOP
						}]),

	% TODO? change getters
	Avail = arbitrator:resource_types(),
	Owned = arbitrator:resource_types(),

	Panel = wxPanel:new(Frame, []),
	SuperSizer = wxBoxSizer:new(?wxVERTICAL),
	MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
	RequestSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
										[{label, "Request"}]),
	OfferSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
										[{label, "Offer"}]),

	ReqPanel1 = wxPanel:new(Panel, []),
	OffPanel1 = wxPanel:new(Panel, []),
	ReqPanel2 = wxPanel:new(Panel, []),
	OffPanel2 = wxPanel:new(Panel, []),

	_Request = wxChoice:new(ReqPanel1, ?ID_OFFER_R, [{choices, Avail}]),
	_Offer = wxChoice:new(OffPanel1, ?ID_OFFER_O, [{choices, Owned}]),

	RQ = wxSpinCtrl:new(ReqPanel2, [{id, ?ID_OFFER_RQ}]),
	OQ = wxSpinCtrl:new(OffPanel2, [{id, ?ID_OFFER_OQ}]),

	EscortPanel = wxPanel:new(Panel, []),
	EscortSpin = wxSpinCtrl:new(EscortPanel, [{id, ?ID_ESCORT_SPIN2}]),
	wxSpinCtrl:setRange(EscortSpin, 0, arbitrator:get_escorts()),

	wxSpinCtrl:setRange(RQ, 1, 100),
	wxSpinCtrl:setRange(OQ, 1, 100),

	CancelPanel = wxPanel:new(Panel, []),
	ConfirmPanel = wxPanel:new(Panel, []),
	ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
	EscortSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Escorts"}]),

	_Cancel = wxButton:new(CancelPanel, ?wxID_DELETE, [{label, "Cancel"}]),
	_Confirm = wxButton:new(ConfirmPanel, ?wxID_APPLY, [{label, "Confirm"}]),

	wxWindow:connect(Panel, command_button_clicked),

	Options = [{border, 8}, {proportion, 0}, {flag, ?wxALL bor ?wxEXPAND}],
	wxSizer:add(EscortSizer, EscortPanel, Options),
	wxSizer:add(ButtonSizer, CancelPanel, Options),
	wxSizer:add(ButtonSizer, ConfirmPanel, Options),
	wxSizer:add(ButtonSizer, EscortSizer),

	wxSizer:add(RequestSizer, ReqPanel1, Options),
	wxSizer:add(RequestSizer, ReqPanel2, Options),
	wxSizer:add(OfferSizer, OffPanel1, Options),
	wxSizer:add(OfferSizer, OffPanel2, Options),

	wxSizer:add(MainSizer, RequestSizer, Options),
	wxSizer:add(MainSizer, OfferSizer, Options),
	wxSizer:add(SuperSizer, MainSizer, Options),
	wxSizer:add(SuperSizer, ButtonSizer, Options),

	wxPanel:setSizer(Panel, SuperSizer),
	wxFrame:center(Frame),
	wxFrame:show(Frame).

% helper to create a subwindow with a panel and sizer
create_subwindow(Parent, BoxLabel, Funs) ->
	Panel = wxPanel:new(Parent),
	Sz	  = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, BoxLabel}]),
	wxPanel:setSizer(Panel, Sz),
	Ctrls = [Fun(Panel) || Fun <- Funs],
	[wxSizer:add(Sz, Ctrl, [{proportion, 1}, {flag, ?wxEXPAND}]) 
	 || Ctrl <- Ctrls],
	{Panel, Ctrls, Sz}.

% insert columns into a list control
cols_to_listctrl(_, []) -> ok;
cols_to_listctrl(Ctrl, [H|T]) ->
	{N, S} = H,
	wxListCtrl:insertColumn(Ctrl, N, S),
	cols_to_listctrl(Ctrl, T).

% create a generic list control with the REPORT layout
create_list_ctrl(Parent, L, W, H) ->
	Panel = wxPanel:new(Parent, []),
	ListCtrl = wxListCtrl:new(Panel, [{style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}, {size, {W, H}}]),
	cols_to_listctrl(ListCtrl, L),
	{Panel, ListCtrl}.
create_list_ctrl(Parent, L) -> create_list_ctrl(Parent, L, 300, 100).

% add a message without formatting, automatically appending a newline
add_message(State, M) ->
	S = M ++ "~n",
	format(State#state.log, S, []).

% the following methods update the information tables
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

% send a private message to another player
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
	if Ret == ?wxID_OK -> % don't send messages if the user didn't press OK
		Msg = wxTextEntryDialog:getValue(Dialog),
		arbitrator:send_private_message(Recipient, Msg);
	true -> true
	end,
	wxDialog:destroy(Dialog),
	ok.

% connect to another node dialog
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

% set name dialog
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

% broadcast a trade request dialog
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

% select a resource to harvest dialog
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
	%format(State#state.log, "~p ~n", [Choice]),
	Choice.

% Not currently in use due to runtime errors on Linux
add_death_ray(_State) ->
	ok.
%	WMB = wxWindow:findWindowById(?ID_MENUBAR),
%	format(State#state.log, "WMB: ~p ~n", [WMB]),
%	MB = wx:typeCast(WMB, wxMenuBar),
%	format(State#state.log, "MB: ~p ~n", [MB]),
%	Menu = wxMenuBar:findMenu(MB, "Mission"),
%	wxMenu:append(Menu, ?ID_ACTIVATE_RAY, "Activate &Death Ray..."),
%	wxWindow:refresh(Menu).

% utility dialog for selecting another node
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

% useful for debugging, but not really used
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
	?wxID_APPLY -> % user is trying to make an offer to another player
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
		EW = wxWindow:findWindowById(?ID_ESCORT_SPIN2),
		EC = wx:typeCast(EW, wxSpinCtrl),
		Escorts = wxSpinCtrl:getValue(EC),
		wxWindow:destroy(W0),
		N = node_d(State, "Make offer:"),
		if N =/= none, N =/= [], W =/= "", H =/= "" ->
			arbitrator:offer(N, W, Q1, H, Q2, Escorts);
		true -> true
		end,
		{noreply, State};
	?wxID_DELETE -> % user cancelled sending an offer
		W0 = wxWindow:findWindowById(?ID_OFFER_WIN),
		wxWindow:destroy(W0),
		{noreply, State};
	?ID_RETRACT -> % user retracted a pending offer
		W0 = wxWindow:findWindowById(?ID_CANCEL_OFFER),
		OLW = wxWindow:findWindowById(?ID_MY_OFFERS),
		OL = wx:typeCast(OLW, wxListCtrl),
		Sel = wxListCtrl:getNextItem(OL, -1, [{geometry, ?wxLIST_NEXT_ALL}, {state, ?wxLIST_STATE_SELECTED}]),
		if Sel =/= -1 ->
			Arg = wxListCtrl:getItemText(OL, Sel),
			format(State#state.log, "Cancelling offer to ~p ~n", [Arg]),
		   	arbitrator:cancel_offer(Arg);
		true -> true
		end,
		wxWindow:destroy(W0),
		{noreply, State};
	?ID_ACCEPT_TRADE -> % user accepted an offer, initiating a trade/cargo mission
		W0 = wxWindow:findWindowById(?ID_TRADE),
		OLW = wxWindow:findWindowById(?ID_ACCEPT_OFFER),
		OL = wx:typeCast(OLW, wxListCtrl),
		Sel = wxListCtrl:getNextItem(OL, -1, [{geometry, ?wxLIST_NEXT_ALL}, {state, ?wxLIST_STATE_SELECTED}]),
		WS = wxWindow:findWindowById(?ID_ESCORT_SPIN),
		Spin = wx:typeCast(WS, wxSpinCtrl),
		Escorts = wxSpinCtrl:getValue(Spin),
		if Sel =/= -1 ->
			Arg = wxListCtrl:getItemText(OL, Sel),
			format(State#state.log, "Initiating trade mission with ~p ~n", [Arg]),
		   	arbitrator:accept_offer(Arg, Escorts);
		true -> true
		end,
		wxWindow:destroy(W0),
		{noreply, State};
	?ID_CLOSE_TRADE -> % user closed the trade mission window
		W0 = wxWindow:findWindowById(?ID_TRADE),
		wxWindow:destroy(W0),
		{noreply, State};
	?ID_CLOSE -> % user closed the cancel offer window
		W0 = wxWindow:findWindowById(?ID_CANCEL_OFFER),
		wxWindow:destroy(W0),
		{noreply, State};
	?ID_BUILD_HARVESTER -> % build a harvester
		arbitrator:build("Harvester"),
		{noreply, State};
	?ID_BUILD_CARGO_SHIP -> % build a cargo ship
		arbitrator:build("Cargo ship"),
		{noreply, State};
	?ID_HARVEST_RARE -> % shortcut button to harvest metals
		arbitrator:harvest("Rare"),
		{noreply, State};
	?ID_HARVEST_METALS -> % shortcut button to harvest metals
		arbitrator:harvest("Metals"),
		{noreply, State};
	_ -> % useful for debugging
		format(State#state.log, "Unhandled button press: #~p ~n", [Id]),
		{noreply, State}
	end;
handle_event(#wx{id = Id,
		 event = #wxCommand{type = command_menu_selected}},
		 State = #state{}) ->
	case Id of
	?wxID_ABOUT -> % About window :)
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
		Modal = wxMessageDialog:new(State#state.win, AboutString,
							  [{style,
								?wxOK bor
								?wxICON_INFORMATION bor
								?wxSTAY_ON_TOP},
							   {caption, "About"}]),
		wxMessageDialog:showModal(Modal),
		{noreply, State};
	?wxID_EXIT -> % quit
		{stop, normal, State};
	?ID_CONNECT -> % open connect dialog
		connect_d(State),
		{noreply, State};
	?ID_BROADCAST -> % broadcast demand
		{W, H} = broadcast_d(State),
		if W =/= none, H =/= none ->
			arbitrator:request_trade(W,H);
			%format(State#state.log, "Broadcasting need for ~p, offering ~p~n", [W,H]);
		true -> true
		end,
		{noreply, State};
	?ID_MESSAGE -> % send private message
		N = node_d(State, "Send to:"),
		if N =/= none, N =/= [] ->
			send_message(State,N);
		true -> true
		end,
		{noreply, State};
	?ID_CANCEL_OFFER -> % cancel a pending offer
		cancel_offer(State),
		{noreply, State};
	?ID_SEND_OFFER -> % send an offer
		make_offer(State),
		{noreply, State};
	?ID_ACTIVATE_RAY -> % use the death ray!!
		arbitrator:destroy_everything(),
		{noreply, State};
	?ID_TRADE -> % embark on trade mission
		accept_offer(State),
		{noreply, State};
	?ID_CLEAR_REQUESTS -> % remove old demands
		arbitrator:clear_trade_requests(),
		{noreply, State};
	?ID_SPY -> % send a spy drone
		N = node_d(State, "Spy on:"),
		if N =/= none, N =/= [] ->
			   arbitrator:send_spy_drone(N);
	   true -> true
		end,
		{noreply, State};
	?ID_IDENTIFY -> % set your node and host name
		Val = identify_d(State),
		if Val =/= none -> 
			format(State#state.log, "You are now known as: ~p ~n", [Val]);
		true -> true
		end,
		{noreply, State};
	?ID_BUILD_SPY -> % build a spy drone
		arbitrator:build("Spy drone"),
		{noreply, State};
	?ID_HARVESTER -> % build a harvester
		arbitrator:build("Harvester"),
		{noreply, State};
	?ID_CARGO_SHIP -> % build a cargo ship
		arbitrator:build("Cargo ship"),
		{noreply, State};
	?ID_ESCORT -> % build an escort
		arbitrator:build("Escort"),
		{noreply, State};
	?ID_DEATH_RAY -> % build a death ray
		arbitrator:build("Death Ray"),
		{noreply, State};
	?ID_HARVEST -> % harvest a resource
		Resource = dialog_harvest_rsrc(State),
		if Resource =/= none ->
			arbitrator:harvest(Resource);
		true -> true
		end,
		{noreply, State};
	% Useful for debugging, and unimplemented features :)
	_ ->
		format(State#state.log, "Unhandled event: #~p ~n", [Id]),
		{noreply, State}
	end;
% We aren't using right-clicking in this game
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

% bridge between abritrator, uses the refresher
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
		{acquire, death_ray} ->
			add_death_ray(State);
		die ->
			exit(ok)
	end,
	handle_update(State).

% bridge between refresher and client
notify(L) ->
	refresher ! L.

