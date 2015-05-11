-module(client).
-export([start/0]).
-include_lib("wx/include/wx.hrl").
-include_lib("wx/src/wxe.hrl").
-define(ID_DEATH_RAY, 101).
-define(ID_HARVEST, 102).
-define(ID_TRADE, 103).
-define(ID_BROADCAST, 104).
-define(ID_MESSAGE, 105).
-define(ID_EXIT, 106).
-define(ID_ABOUT, 107).

start() ->
	Wx = wx:new(),
	Frame = wx:batch(fun() -> create_window(Wx) end),
	wxWindow:show(Frame),
	loop(Frame),
	wx:destroy(),
	ok.

create_window(Wx) -> 
	% initialize the main window
	F=wxFrame:new(Wx, -1, "Kardashiev Client"),

    wxFrame:createStatusBar(F,[]),
    wxFrame:connect(F, close_window),

	MenuBar = wxMenuBar:new(),
	wxFrame:setMenuBar(F, MenuBar),
	% Menu bar items
	BuildMn = wxMenu:new(),
	MissionMn = wxMenu:new(),
	CommsMn = wxMenu:new(),
	GameMn = wxMenu:new(),

	% Build menu
	DeathRay = wxMenuItem:new([{id, ?ID_DEATH_RAY}, {text, "&Death Ray"}]),
	wxMenu:append(BuildMn, DeathRay),

	% Mission menu
	Harvest = wxMenuItem:new([{id, ?ID_HARVEST}, {text, "&Harvest"}]),
	Trade = wxMenuItem:new([{id, ?ID_TRADE}, {text, "&Trade"}]),
	wxMenu:append(MissionMn, Harvest),
	wxMenu:append(MissionMn, Trade),

	% Communication menu
	Broadcast = wxMenuItem:new([{id, ?ID_BROADCAST}, {text, "&Broadcast"}]),
	Message = wxMenuItem:new([{id, ?ID_MESSAGE}, {text, "&Message"}]),
	wxMenu:append(CommsMn, Broadcast),
	wxMenu:append(CommsMn, Message),

	% Game menu
	Quit = wxMenuItem:new([{id, ?ID_EXIT}, {text, "&Quit"}]),
	About = wxMenuItem:new([{id, ?ID_ABOUT}, {text, "&About"}]),
	wxMenu:append(GameMn, About),
	wxMenu:appendSeparator(GameMn),
	wxMenu:append(GameMn, Quit),

    ok = wxFrame:connect(F, command_menu_selected), 

	wxMenuBar:append(MenuBar, BuildMn, "&Build"),
	wxMenuBar:append(MenuBar, MissionMn, "&Mission"),
	wxMenuBar:append(MenuBar, CommsMn, "&Communicate"),
	wxMenuBar:append(MenuBar, GameMn, "&Game"),

    LeftSplitter  = wxSplitterWindow:new(F, [{style, ?wxSP_NOBORDER}]),

    wxSplitterWindow:setSashGravity(LeftSplitter,  0.50),

	{RSPanel, _, _} = create_subwindow(LeftSplitter, "Foo", []),
	{TCPanel, _, _} = create_subwindow(LeftSplitter, "Foo", []),

	wxSplitterWindow:splitVertically(LeftSplitter, RSPanel, TCPanel, [{sashPosition, 200}]),

	Resources = create_resource_ctrl(RSPanel, [{style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
	Contacts = create_trade_ctrl(TCPanel, [{style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),

    ok = wxFrame:setStatusText(F, "Kardashiev game initialized",[]),

	F.

loop(Frame) ->
	receive
		#wx{event=#wxClose{}} -> 
			wxFrame:destroy(Frame),
			ok;
		#wx{id=?ID_EXIT, event=#wxCommand{type=command_menu_selected}} -> 
			wxFrame:destroy(Frame),
			ok;
		#wx{id=?ID_ABOUT, event=#wxCommand{type=command_menu_selected}} -> 
			dialog(?ID_ABOUT, Frame),
			loop(Frame);
		Msg ->
			io:format("Got ~p ~n", [Msg]),
			loop(Frame)
	after 1000 ->
		io:fwrite("."),
		loop(Frame)
	end.

dialog(?ID_ABOUT, Frame) ->
	Str = string:join(["Kardashiev Space Trading & Strategy Game.\n",
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
	MD = wxMessageDialog:new(Frame,
		Str,
		[{style, ?wxOK bor ?wxICON_INFORMATION}, 
		{caption, "About Kardashiev Game"}]),

	wxDialog:showModal(MD),
	wxDialog:destroy(MD).

-define(FIRST_COL, 0).
-define(SECOND_COL, 1).
-define(THIRD_COL, 2).
	
create_resource_ctrl(Win, Options) ->
	ListCtrl = wxListCtrl:new(Win, Options),
	wxListCtrl:insertColumn(ListCtrl, ?FIRST_COL, "Resource"),
	wxListCtrl:insertColumn(ListCtrl, ?SECOND_COL, "Quantity"),
	ListCtrl.

create_trade_ctrl(Win, Options) ->
	ListCtrl = wxListCtrl:new(Win, Options),
	wxListCtrl:insertColumn(ListCtrl, ?FIRST_COL, "Name"),
	wxListCtrl:insertColumn(ListCtrl, ?SECOND_COL, "Have"),
	wxListCtrl:insertColumn(ListCtrl, ?THIRD_COL, "Want"),
	ListCtrl.

create_subwindow(Parent, BoxLabel, Funs) ->
    Panel = wxPanel:new(Parent),
    Sz    = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, BoxLabel}]),
    wxPanel:setSizer(Panel, Sz),
    Ctrls = [Fun(Panel) || Fun <- Funs],
    [wxSizer:add(Sz, Ctrl, [{proportion, 1}, {flag, ?wxEXPAND}]) 
     || Ctrl <- Ctrls],
    {Panel, Ctrls, Sz}.


