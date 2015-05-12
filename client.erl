-module(client).
-export([start/0, start/1, start_link/0, start_link/1, format/3, 
	 init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-include_lib("wx/include/wx.hrl").

-behaviour(wx_object).

-record(state, {win, log, resources, contacts}).

-define(ID_DEATH_RAY, 101).
-define(ID_HARVEST, 102).
-define(ID_TRADE, 103).
-define(ID_BROADCAST, 104).
-define(ID_MESSAGE, 105).
-define(ID_CONNECT, 106).

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
	wxMenu:append(Build, ?ID_DEATH_RAY, "&Death Ray"),
	Mission = wxMenu:new([]),
	wxMenu:append(Mission, ?ID_HARVEST, "&Harvest"),
	wxMenu:append(Mission, ?ID_TRADE, "&Trade"),
	Comms	= wxMenu:new([]),
	wxMenu:append(Comms, ?ID_BROADCAST, "&Broadcast"),
	wxMenu:append(Comms, ?ID_MESSAGE, "&Message"),
	wxMenu:appendSeparator(Comms),
	wxMenu:append(Comms, ?ID_CONNECT, "&Connect"),
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

	TopSplitter = wxSplitterWindow:new(Frame, [{style, ?wxSP_NOBORDER}]),
	UpperSplitter = wxSplitterWindow:new(TopSplitter, [{style, ?wxSP_NOBORDER}]),

	wxSplitterWindow:setSashGravity(TopSplitter,   0.5),
	wxSplitterWindow:setSashGravity(UpperSplitter, 0.6),

	% ...
    {ResourcePanel, [], _} = create_subwindow(UpperSplitter, "Resources", []),
    {ContactPanel, [], _} = create_subwindow(UpperSplitter, "Contacts", []),

	%% UpperSplitter:
	wxSplitterWindow:splitVertically(UpperSplitter, ResourcePanel, ContactPanel, [{sashPosition,600}]),

	Resources = create_resource_ctrl(ResourcePanel, [{style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
	Contacts = create_trade_ctrl(ContactPanel, [{style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
	
	%% TopSplitter: 
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
	
	wxSplitterWindow:splitHorizontally(TopSplitter, UpperSplitter, EvPanel, [{sashPosition,-100}]),

	wxFrame:show(Frame),

	State = #state{win=Frame, log=EvCtrl, resources=Resources, contacts=Contacts},

    wxSplitterWindow:setSashGravity(TopSplitter,   1.0),
    wxSplitterWindow:setSashGravity(UpperSplitter, 0.0),
    wxSplitterWindow:setMinimumPaneSize(TopSplitter, 1),
    wxSplitterWindow:setMinimumPaneSize(UpperSplitter, 1),

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
	ListCtrl = wxListCtrl:new(Win, Options),
	wxListCtrl:insertColumn(ListCtrl, 0, "Name"),
	wxListCtrl:insertColumn(ListCtrl, 1, "Have"),
	wxListCtrl:insertColumn(ListCtrl, 2, "Want"),
	ListCtrl.

create_resource_ctrl(Win, Options) ->
	ListCtrl = wxListCtrl:new(Win, Options),
	wxListCtrl:insertColumn(ListCtrl, 0, "Resource"),
	wxListCtrl:insertColumn(ListCtrl, 1, "Quantity"),
	ListCtrl.

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
		format(State#state.log, "Unimplemented CONNECT event: #~p ~n", [Id]),
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
