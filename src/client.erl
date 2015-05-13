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

	Sizer = wxBoxSizer:new(?wxHORIZONTAL),

	TopSplitter   = wxSplitterWindow:new(Frame, [{style, ?wxSP_NOBORDER}]),
	UpperSplitter = wxSplitterWindow:new(TopSplitter, [{style, ?wxSP_NOBORDER}]),
	ResSplitter   = wxSplitterWindow:new(UpperSplitter, [{style, ?wxSP_NOBORDER}]),
	TrdSplitter   = wxSplitterWindow:new(UpperSplitter, [{style, ?wxSP_NOBORDER}]),
% 
	{ResourcePanel, [], _} = create_subwindow(ResSplitter, "Resources", []),
	{ShipPanel, [], _}     = create_subwindow(ResSplitter, "Ships", []),

	{ContactPanel, [], _}  = create_subwindow(TrdSplitter, "Contacts", []),
	{OfferPanel, [], _}    = create_subwindow(TrdSplitter, "Offers", []),

	wxSplitterWindow:splitVertically(UpperSplitter, ResSplitter, TrdSplitter, [{sashPosition, 500}]),
	wxSplitterWindow:splitHorizontally(ResSplitter, ResourcePanel, ShipPanel, [{sashPosition, 300}]),
	wxSplitterWindow:splitHorizontally(TrdSplitter, ContactPanel, OfferPanel, [{sashPosition, 300}]),

	{Resources, RSZ} = create_resource_ctrl(ResourcePanel, [{style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
	{Contacts, CSZ}  = create_trade_ctrl(ContactPanel, [{style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),

	{Ships, SSZ}     = create_ship_ctrl(ShipPanel, [{style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
	{Offers, OSZ}    = create_offer_ctrl(OfferPanel, [{style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),

	wxSizer:add(Sizer, RSZ),
	wxSizer:addSpacer(Sizer, 20),
	wxSizer:add(Sizer, CSZ),
	wxSizer:add(Sizer, SSZ),
	wxSizer:add(Sizer, OSZ),

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
	wxSplitterWindow:splitHorizontally(TopSplitter, UpperSplitter, EvPanel, [{sashPosition, 600}]),
	
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

	wxFrame:show(Frame),

	{Frame, State}.

%% Helpers
create_subwindow(Parent, BoxLabel, Funs) ->
	Panel = wxPanel:new(Parent),
	Sz	  = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, BoxLabel}]),
	wxPanel:setSizer(Panel, Sz),
	Ctrls = [Fun(Panel) || Fun <- Funs],
	[wxSizer:add(Sz, Ctrl, [{proportion, 1}, {flag, ?wxEXPAND}]) 
	 || Ctrl <- Ctrls],
	{Panel, Ctrls, Sz}.

create_trade_ctrl(Win, Options) ->
	Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Win, [{label, "Contacts"}]),
	ListCtrl = wxListCtrl:new(Win, Options),
	wxListCtrl:insertColumn(ListCtrl, 0, "Name"),
	wxListCtrl:insertColumn(ListCtrl, 1, "Have"),
	wxListCtrl:insertColumn(ListCtrl, 2, "Want"),
	wxSizer:add(Sizer, ListCtrl),
	{ListCtrl, Sizer}.

create_ship_ctrl(Win, Options) ->
	Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Win, [{label, "Ships"}]),
	ListCtrl = wxListCtrl:new(Win, Options),
	wxListCtrl:insertColumn(ListCtrl, 0, "Ship"),
	wxListCtrl:insertColumn(ListCtrl, 1, "Quantity"),
	wxSizer:add(Sizer, ListCtrl),
	{ListCtrl, Sizer}.

create_resource_ctrl(Win, Options) ->
	Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Win, [{label, "Resources"}]),
	ListCtrl = wxListCtrl:new(Win, Options),
	wxListCtrl:insertColumn(ListCtrl, 0, "Resource"),
	wxListCtrl:insertColumn(ListCtrl, 1, "Quantity"),
	wxSizer:add(Sizer, ListCtrl),
	{ListCtrl, Sizer}.

create_offer_ctrl(Win, Options) ->
	Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Win, [{label, "Offers"}]),
	ListCtrl = wxListCtrl:new(Win, Options),
	wxListCtrl:insertColumn(ListCtrl, 0, "Name"),
	wxListCtrl:insertColumn(ListCtrl, 1, "Qty (want)"),
	wxListCtrl:insertColumn(ListCtrl, 2, "Want"),
	wxListCtrl:insertColumn(ListCtrl, 3, "Offer"),
	wxListCtrl:insertColumn(ListCtrl, 4, "Qty (offer)"),
	wxSizer:add(Sizer, ListCtrl),
	{ListCtrl, Sizer}.

add_message(State, M) ->
	S = M ++ "~n",
	format(State#state.log, S, []).

update_offers(State, Contacts) ->
	ListCtrl = State#state.offers,
	wxListCtrl:deleteAllItems(ListCtrl),
	insert_contact(ListCtrl, Contacts),
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
	true -> true
	end,
	wxDialog:destroy(Dialog),
	ok.

dialog_harvest_rsrc(State) -> 
	Frame = State#state.win,
	Resources = arbitrator:resource_types(),
	Dialog = wxSingleChoiceDialog:new(Frame,
									  "Harvest resource:",
									  "Harvest",
									  Resources),
	wxDialog:showModal(Dialog),
	Choice = wxSingleChoiceDialog:getStringSelection(Dialog),
	wxDialog:destroy(Dialog),
	format(State#state.log, "~p ~n", [Choice]),
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
	?ID_IDENTIFY ->
		identify_d(State),
		format(State#state.log, "You are now known as: #~p ~n", [Id]),
		{noreply, State};
	?ID_HARVESTER ->
		arbitrator:build("Harvester"),
		{noreply, State};
	?ID_CARGO_SHIP ->
		arbitrator:build("Cargo Ship"),
		{noreply, State};
	?ID_ESCORT ->
		arbitrator:build("Escort"),
		{noreply, State};
	?ID_DEATH_RAY ->
		arbitrator:build("Death Ray"),
		{noreply, State};
	?ID_HARVEST ->
		Resource = dialog_harvest_rsrc(State),
		arbitrator:harvest(Resource),
		{noreply, State};
	_ ->
		format(State#state.log, "Unhandled event: #~p ~n", [Id]),
		{noreply, State}
	end;
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

