%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    CAN frame viewer
%%% @end
%%% Created : 22 Nov 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(candy).
-behaviour(epxw).

%% API
-export([start0/0]).  %% make app image target
-export([start/0, start/1]).
-export([status/0]).
-export([install/0, install_cmds/0]).

%% -export([start_link/0, start_link/1]).

-export([init/1,
	 configure/2,
	 key_press/2,
	 key_release/2,
	 button_press/2,
	 button_release/2,
	 enter/2,
	 leave/2,
	 focus_in/2,
	 focus_out/2,
	 close/1,
	 draw/3,
	 command/3,
	 select/2,
	 motion/2,
	 menu/2
	]).

%% gen_server callbacks
-export([handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("can/include/can.hrl").
-include_lib("epx/include/epx_menu.hrl").
-include_lib("epx/include/epx_window_content.hrl").

%% -compile(export_all).

-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(GET_STACK(Stacktrace), Stacktrace).
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(GET_STACK(_), erlang:get_stacktrace()).
-endif.

-define(ld(Key, Env, Default),
	proplists:get_value(Key, Env, Default#profile.Key)).

-define(ldc(Scheme, Key, Env, Default),
	epx_profile:color_number(Scheme, ?ld(Key,Env,Default))).

%% profile with default values
-record(profile,
	{
	 scheme                        = logo, %% xterm,
	 %% menu_info
	 menu_font_name                = "Arial",
	 menu_font_size                = 14,
	 menu_font_color               = grey5,   %% light
	 menu_background_color         = grey10,  %% dark
	 menu_border_color             = green
	}).

-define(LAYOUT_BACKGROUND_COLOR, {255,255,255}).   %% white
-define(FRAME_BORDER_COLOR,      {0,0,0}).         %% black border
-define(TEXT_COLOR,              {0,0,0,0}).       %% black text
-define(TEXT_HIGHLIGHT_COLOR,    {0,255,255,255}). %% white hight light
-define(HIGHLIGHT_COLOR1,   {215,48,39}).
-define(HIGHLIGHT_COLOR2,  {240,101,14}).
-define(HIGHLIGHT_COLOR3,  {176,122,78}).
-define(HIGHLIGHT_COLOR4,  {234,234,20}).
-define(HIGHLIGHT_COLOR5,  {94,191,63}).
-define(HIGHLIGHT_COLOR6,  {42,212,57}).
-define(HIGHLIGHT_COLOR7,  {37,217,164}).
-define(HIGHLIGHT_COLOR8,  {50,167,204}).
-define(HIGHLIGHT_COLOR9,  {47,153,207}).
-define(HIGHLIGHT_COLOR10, {70,119,184}).
-define(HIGHLIGHT_COLOR11, {130,82,172}).
-define(HIGHLIGHT_COLOR12, {221,33,140}).
-define(HIGHLIGHT_COLOR13, {205,49,86}).

-define(FONT_NAME, "Courier New").
-define(FONT_SIZE, 14).


-define(APP, ?MODULE).
-define(APPSTR, ?MODULE_STRING).
-define(DOTAPP, [$.|?APPSTR]).
-define(APPPNG, ?APPSTR++".png").
-define(APPDSK, ?APPSTR++".desktop").

-define(verbose(F,A), ok).
%% -define(verbose(F,A), io:format((F),(A))).

-record(fmt,
	{
	 dx :: integer(),   %% position relative #layout.x
	 dy :: integer(),   %% position relative #layout.y
	 width :: non_neg_integer(),
	 height :: non_neg_integer(),
	 hidden = false :: boolean(),
	 type = unsigned :: unsigned | signed | undefined |
			    {enum,tuple()} | {string,string()},
	 field = none :: none | id | len | data | frequency | hide,
	 base = 16 :: 0 | 2 | 8 | 16 | 10,
	 signed = false :: boolean(),
	 ci = 1 :: 1..13,               %% color index in cell_color(I)
	 bits = [] :: [{Pos::non_neg_integer(),Length::non_neg_integer()}]
	}).

%% can frame color
-record(color,
	{
	 background  = ?LAYOUT_BACKGROUND_COLOR,
	 foreground  = ?TEXT_COLOR,               %% text color
	 border      = ?FRAME_BORDER_COLOR,       %% frame border color
	 background1  = ?HIGHLIGHT_COLOR1,
	 foreground1  = ?TEXT_HIGHLIGHT_COLOR
	}).

-record(layout,
	{
	 id  :: integer(),             %% frame id
	 pos :: integer(),             %% list position
	 pos0 :: integer(),            %% original list position
	 src :: integer(),             %% source position when move/sort
	 style = normal :: normal | fixed | collapsed | hidden | deleted,
	 color = #color{} :: #color{}, %% color profile
	 x     :: integer(),           %% x offset
	 y     :: integer(),           %% y offset
	 width :: non_neg_integer(),   %% total width
	 height :: non_neg_integer(),  %% total height
	 format = {} :: tuple()        %% {#fmt{},...}
	}).

-define(ID_FMT_POSITION, 2).

-define(DEFAULT_BITRATES, [1000000, 500000, 250000, 125000]).
-define(DEFAULT_DATARATES, [5000000,4000000,3000000,2000000,
			    1000000,500000,250000,125000]).

-define(WIDTH, 640).
-define(HEIGHT, 480).

-record(if_can,
	{
	 if_mod :: atom(),
	 if_pid :: pid(),
	 if_id  :: integer()
	}).

%% kind of constant elements
-record(s,
	{
	 fd = undefined,                  %% fd mode
	 listen_only = undefined,         %% listen only mode
	 bitrate = undefined,
	 bitrates = list_to_tuple(?DEFAULT_BITRATES),
	 datarate = undefined,
	 datarates =  list_to_tuple(?DEFAULT_DATARATES),

	 if_state  = down,            %% up | down
	 if_param  = #{},             %% current interface params (when up)
	 if_error  = [],              %% interface error code list
	 if_can :: #if_can{},
	 nrows = 30 :: integer(),     %% number or rows shown
	 font   :: epx:epx_font(),
	 %%
	 glyph_width,
	 glyph_height,
	 glyph_ascent,
	 %%
	 frame,          %% ets: #can_frame{}
	 frame_layout,   %% ets: #layout{}
	 frame_counter,  %% ets: ID -> Counter
	 frame_freq,     %% ets: {ID,Time,OldCounter}
	 frame_anim,     %% ets: {ID,FmtPos} -> Counter
	 %% background_color :: epx:epx_color(),
	 menu_profile,
	 row_width     = 0,
	 row_height    = 0,
	 row_pad = 3,
	 next_pos = 1,
	 hide :: epx:epx_pixmap()
	}).

%% dynamic state elements
-record(d,
	{
	 tick :: undefined | reference(),
	 selection,                 %% current selection rect
	 selected = [],             %% selected frames [{FID,Pos}]
	 content :: #window_content{}
	}).


status() ->
    io:format("candy is running\n").

askpass() ->
    filename:join(code:priv_dir(epx), "epx_askpass").

%% runtime install, first time for APPIMAGE
install() ->
    APPIMAGE = os:getenv("APPIMAGE",undefined),
    if APPIMAGE =:= undefined ->
	    ok;    
       true ->
	    Home = os:getenv("HOME"),
	    {ok,Vsn} = application:get_key(?APP, vsn),
	    VsnNL = Vsn++"\n",
	    case file:read_file(filename:join([Home,?DOTAPP,"installed"])) of
		{ok,BinVsn} ->
		    case binary_to_list(BinVsn) of
			Vsn -> ok; %% already installed
			VsnNL -> ok; %% already installed
			_ -> install_(Home, Vsn)
		    end;
		{error,enoent} -> install_(Home,Vsn)
	    end
    end.

%% root should during real run be candy.AppDir
%% config is set to candy.AppDir/candy.config
install_(Home,Vsn) ->
    APPIMAGE = os:getenv("APPIMAGE", ""),
    APPDIR = os:getenv("APPDIR",""),
    {User0,Admin0} = can:install_cmds(),
    {User1,Admin1} = install_cmds_(Home,Vsn),
    Dir = filename:join(Home,?DOTAPP),
    file:make_dir(Dir),
    os:cmd(string:join(User0++User1, ";")),

    log_commands(Dir,["# APPIMAGE", ["echo ",APPIMAGE]]),
    log_commands(Dir,["# APPDIR", ["echo ",APPDIR]]),
    log_commands(Dir,["# USER0"|User0]),
    log_commands(Dir,["# USER1"|User1]),
    if Admin0 =:= [], Admin1 =:= [] ->
	    ok;
       true ->
	    os:cmd("export SUDO_ASKPASS="++askpass()++"; sudo -A sh -c \""++
		       string:join(Admin0++Admin1, ";")++"\""),
	    log_commands(Dir,["# ADMIN0"|Admin0]),
	    log_commands(Dir,["# ADMIN1"|Admin1]),
	    ok
    end.

install_cmds() ->
    Home = os:getenv("HOME"),
    {ok,Vsn} = application:get_key(?APP, vsn),
    install_cmds_(Home,Vsn).

install_cmds_(Home,Vsn) ->
    APPDIR = os:getenv("APPDIR", ""),
    APPIMG = os:getenv("APPIMAGE", ""),
    AppDir = case init:get_argument(root) of
		 {ok,[[APPDIR]]} ->
		     APPDIR;
		 _ -> 
		     code:priv_dir(candy)
	     end,
    {[lists:flatten(Cmd)||Cmd <- install_cmd_(Home, Vsn, AppDir,APPIMG)],
     []}.

install_cmd_(Home, Vsn, AppDir,AppImg) ->
    IconsDir = filename:join([AppDir,"desktop_icons","hicolor"]),
    case file:list_dir(IconsDir) of
	{ok,Sizes} ->
	    lists:append(
	      [
	       [
		["mkdir -p ", filename:join([Home,".local","share","icons","hicolor",Size,"apps"])],
		["cp ", 
		 filename:join([AppDir,"desktop_icons","hicolor",Size,
				"apps",?APPPNG])," ",
		 filename:join([Home,".local","share","icons","hicolor",Size,
				"apps",?APPPNG])]] || Size <- Sizes]);
	_ ->
	    [["echo icondir=",IconsDir]]
    end ++
    [
     ["cat ", filename:join(AppDir, ?APPDSK), 
      " | sed 's%APPIMAGE%",AppImg,"%' > ",
      filename:join([Home, "Desktop", string:titlecase(?APPDSK)])],
     ["mkdir -p ", filename:join(Home,?DOTAPP)],
     ["echo \"", Vsn, "\" > ", filename:join([Home,?DOTAPP,"installed"])]
    ].

%% log each command, one by one
log_commands(Dir, [Cmd|Cmds]) ->
    {ok,Fd} = file:open(filename:join(Dir,"install_log.txt"), [write,append]),
    io:put_chars(Fd, [Cmd,"\n"]),
    file:close(Fd),
    log_commands(Dir, Cmds);
log_commands(_Dir, []) ->
    ok.

%%%===================================================================
%%% API
%%%===================================================================

%% called from make appimage 
start0() ->
    application:load(?APP),
    application:ensure_all_started(?APP),
    start_it([], start).

start() ->
    start([]).
start(Options) ->
    application:load(?APP),
    install(),
    low_latency(),
    application:ensure_all_started(?APP),
    start_it(Options, start).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
%% start_link() ->
%%     start_link([]).
%% start_link(Options) ->
%%     (catch error_logger:tty(false)),
%%     low_latency(),
%%     application:ensure_all_started(?MODULE),
%%     start_it(Options, start_link).

start_it(Options, Start) ->
    epxw:Start(?MODULE,
	       Options,
	       [{title, "Candy"},
		{scroll_horizontal, bottom},  %% none|top|bottom
		{scroll_vertical,   right},   %% none|left|right
		{scroll_bar_color,  cyan},
		{scroll_hndl_color, blue},
		{scroll_bar_size,   14},
		{scroll_hndl_size,  10},
		{left_bar, 0},
		{right_bar, 0},
		{top_bar, 20},
		{width,  ?WIDTH},
		{height, ?HEIGHT},
		{view_width,?WIDTH},
		{view_height,?HEIGHT-(20+18)}]).
    
%% make sure CANUSB is on full speed (+ other FTDI serial devices)
low_latency() ->
    lists:foreach(
      fun(UsbDev) ->
	      %% ignore output since only FTDI devices will return ok
	      os:cmd("setserial "++UsbDev++" low_latency")
      end, filelib:wildcard("/dev/ttyUSB*")).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Options) ->
    process_flag(trap_exit, true),
    Env = Options ++ application:get_all_env(candy),
    can_router:attach(),
    can_router:error_reception(on),
    can_router:if_state_supervision(on),
    S0 = #s{},
    {ok,Font} = epx_font:match([{name,?FONT_NAME},{size,?FONT_SIZE}]),
    epx_gc:set_font(Font),
    {W,H}  = epx_font:dimension(Font,"0"),

    Profile = #profile{},
    MProfile = create_menu_profile(Profile),

    Window = epxw:window(),
    epx:window_enable_events(Window, no_auto_repeat),

    RowHeight = H + 2,
    NRows = S0#s.nrows + 2,  %% 2 collapsed frame rows
    _Height = NRows*(RowHeight+S0#s.row_pad) - S0#s.row_pad,
    %% FORMAT= ID X|R|E L 01 23 45 67 01 23 45 67
    %% maximum width = 
    %%   ID: 3FF (11-bit) | 1FFFFFFF (29-bit)
    %% Data: 64 bit : 8*8 (16 char) | 64*1 (64 char) | X|R
    %% Given 8 byte groups in base 2
    Width = 1*(8*W+4+2) + 3*(1*W+4+2) + 1*(1*W+4+2) + 8*(6+8*W+4+2) - 2,
    BitRates0 = proplists:get_value(bitrates, Env, ?DEFAULT_BITRATES),

    {IF,
     [{listen_only,LISTEN},{fd,FD},
      {bitrate,BitRate}, {bitrates, BitRates},
      {datarate,DataRate}, {datarates,DataRates}]} =
	case get_can_if() of
	    {ok,IFCan=#if_can{if_mod=Mod,if_pid=Pid}} when is_pid(Pid) ->
		{IFCan, Mod:getopts(Pid, [listen_only, fd, 
					  bitrate, bitrates, 
					  datarate, datarates])};
	    {error,_} ->
		{#if_can{if_mod=undefined,if_id=0,if_pid=undefined},
		 [{listen_only, false},
		  {fd, false},
		  {bitrate, hd(BitRates0)},
		  {bitrates, BitRates0},
		  {datarate, undefined},
		  {datarates, ?DEFAULT_DATARATES}]}
	end,

    S1 = S0#s {
	   listen_only = LISTEN,
	   fd = FD,
	   bitrate = BitRate,
	   bitrates = sort_rates(BitRates),
	   datarate = DataRate,
	   datarates = sort_rates(DataRates),
	   if_can = IF,
	   font   = Font,
	   glyph_width  = W,
	   glyph_height = H,
	   glyph_ascent = epx:font_info(Font, ascent),
	   frame = ets:new(frame, [{keypos,#can_frame.id}]),
	   frame_layout  = ets:new(frame_layout, [{keypos,#layout.id}]),
	   frame_counter = ets:new(frame_counter, []),
	   frame_anim    = ets:new(frame_anim, []),
	   frame_freq    = ets:new(frame_freq, []),
	   menu_profile  = MProfile,
	   row_width     = Width,
	   row_height    = RowHeight,
	   hide = hide_pixels()
	  },
    State = {S1, #d{} },
    Model = proplists:get_value(model, Env, any),
    State1 = load_frame_layout(Model, State),
    set_status(State1),
    {ok, State1}.

create_menu_profile(Profile) ->
    #menu_profile {
       scheme           = Profile#profile.scheme,
       font_name        = Profile#profile.menu_font_name,
       font_size        = Profile#profile.menu_font_size,
       font_color       = Profile#profile.menu_font_color,
       background_color = Profile#profile.menu_background_color,
       border_color     = Profile#profile.menu_border_color
      }.

configure(_Rect,State) ->
    ?verbose("CONFIGURE: ~w\n", [_Rect]),
    State.

key_press(_Event={_, _Sym, _Mod, _Code}, State={_S,_D}) ->
    ?verbose("KEY_PRESS: ~w, mod=~p\n", [_Event, _Mod]),
    State.

key_release(_Event={_, _Sym, _Mod, _Code}, State={_S,_D}) ->
    ?verbose("KEY_RELEASE: ~w\n", [_Event]),
    State.

button_press(_Event={button_press, Buttons,XY={_X,_Y}}, State) ->
    ?verbose("BUTTON_PRESS: ~w\n", [_Event]),
    case Buttons of
	[left] ->
	    case layout_from_coord(XY, State) of
		false ->
		    deselect(State, []);
		Layout ->
		    cell_hit(XY,Layout,State)
	    end;
	_ ->
	    State
    end.

button_release(_Event={_,_Buttons,{_X,_Y}},State) ->
    ?verbose("BUTTON_RELEASE: ~w\n", [_Event]),
    State.

enter(_Event, State) ->
    ?verbose("ENTER: ~w\n", [_Event]),
    State.

leave(_Event, State) ->
    ?verbose("LEAVE: ~w\n", [_Event]),
    State.

focus_in(_Event, State) ->
    ?verbose("FOCUS_IN: ~w\n", [_Event]),
    State.

focus_out(_Event, State) ->
    ?verbose("FOCUS_OUT: ~w\n", [_Event]),
    State.

close(State={_S,_D}) ->
    %% ?verbose("Got window close\n", []),
    erlang:halt(0),   %% temporary hack?
    State.

draw(Pixels, Area, State) ->
    ?verbose("DRAW: Area = ~p\n", [Area]),
    redraw_all(Pixels, Area, State),
    State.


menu({menu,_Pos}, State={S,_D}) ->
    ?verbose("MENU: Pos = ~p\n", [_Pos]),
    MProfile = S#s.menu_profile,
    Menu =
	case S#s.if_can of
	    #if_can{if_mod=Mod,if_pid=Pid} when is_pid(Pid) ->
		[{listen_only,LISTEN},{fd,FD},
		 {bitrate,BitRate}, {bitrates, BitRates},
		 {datarate,DataRate}, {datarates,DataRates}] =
		    Mod:getopts(Pid, [listen_only, fd, 
				      bitrate, bitrates, 
				      datarate, datarates]),
		LISTEN_ON = if LISTEN -> "ON";
			       not LISTEN -> "OFF";
			       true -> "N/A"
			    end,
		FD_ON = if FD -> "ON";
			   not FD -> "OFF";
			   true -> "N/A"
			end,
		BitRates1 = sort_rates(BitRates),
		DataRates1 = sort_rates(DataRates),
		[{"Listen="++LISTEN_ON, "Ctrl+L"},
		 {"Fd="++FD_ON, "Ctrl+F"}] ++
		    [{rate_to_list(R,BitRate), "Alt+"++
			  integer_to_list(I,16)} ||
			{R,I} <- lists:zip(BitRates1,
					   lists:seq(1, length(BitRates1)))]++
		    if FD ->
			    [{rate_to_list(R,DataRate), "Ctrl+"
			      ++integer_to_list(I,16)} ||
				{R,I} <- lists:zip(DataRates1, 
						   lists:seq(1, length(DataRates1)))];
		       true -> []
		    end;
	    _ ->
		[{"Listen=N/A", "Ctrl+L"},
		 {"Fd=N/A",     "Ctrl+F"},
		 {" 1000k",      "Alt+1"},
		 {" 500k",       "Alt+2"},
		 {" 250k",       "Alt+3"},
		 {" 125k",       "Alt+4"}
		]
	end,
    CanMenu = epx_menu:create(MProfile#menu_profile{background_color=green},
			      Menu),
    {reply, CanMenu, State}.

		
rate_to_list(Rate,CurrentRate) ->
    RateK = Rate div 1000,
    RateM = Rate rem 1000,
    Mark = if Rate =:= CurrentRate -> ">";
	      true -> " "
	   end,
    if RateM =:= 0 ->
	    Mark++integer_to_list(RateK)++"k";
       true ->
	    Mark++integer_to_list(RateK) ++ "." ++
		hd(integer_to_list(RateM)) ++ "k"
    end.

select({_Phase,Rect}, {S,D}) ->
    ?verbose("SELECT: ~w\n", [Rect]),
    ?verbose("State = S=~p, D=~p\n", [S, D]),
    {S, D#d { selection = Rect }}.

motion(_Event={motion,_Button,_Pos}, State) ->
    ?verbose("MOTION: ~w\n", [_Event]),
    State.

%%
%% Commands on Selected elements:
%%   X              Hexa decimal format
%%   D              Decimal format
%%   B              Binary format
%%   O              Octal format
%%   C              Color format
%%   --
%%   G              Group selected bits
%%   Shift+G        Ungroup selected bits
%%   H              Sort Low -> High frames by frame id
%%   Ctrl+H         Sort High -> Low frames by frame id
%%   1-8            Split in groups of 1 to 8 bits
%%   Alt+1-9        Set bitrate 
%%   Alt+up         Bitrate up
%%   Alt+down       Bitrate down
%%   T              Swap groups (not implemented yet)
%%   Ctrl+L         Listen mode toggle
%%   Ctrl+F         FD allow toggle
%%   Ctrl+1-9[A-E]  Set datarate (FD)
%%   Ctrl+up        Datarate up
%%   Ctrl+down      Datarate down
%%   Ctrl+S         Save information to /home/$USER/candy.txt
%%
%% Global commands
%%   Q              Quit application
%%   up             Arrow up, scroll up
%%   down           Arrow down, scroll down
%%   pageup         Page up, scroll one page up
%%   pagedown       Page down, scroll one page down
%%
command(Key, Mod, State={_,D}) ->
    ?verbose("COMMAND: key=~p, mod=~p\n", [Key, Mod]),
    case command(Key, D#d.selected, Mod, State) of
	Reply = {reply, _SymMod, _State1} ->
	    Reply;
	State1 ->
	    {noreply, State1}
    end.
	     

command($x, Selected, _Mod, State) ->
    set_base(Selected, 16, State);
command($d, Selected, _Mod, State) ->
    set_base(Selected, 10, State);
command($o, Selected, _Mod, State) ->
    set_base(Selected, 8, State);
command($b, Selected, _Mod, State) ->
    set_base(Selected, 2, State);
command($c, Selected, _Mod, State) ->
    set_base(Selected, c, State);
command($G, Selected, Mod, State) when Mod#keymod.shift ->
    Fs = lists:usort([FID || {FID,_} <- Selected]),
    lists:foldl(fun(FID,Si) -> split_half_fid(FID,Selected,Si) end, State, Fs);
command($g, Selected, _Mod, _State = {S,D}) ->
    Fs = lists:usort([FID || {FID,_} <- Selected]),
    %% select min pos foreach FID and keep as selected
    Selected1 = lists:foldl(
		  fun(FID, Acc) ->
			  MinPos = lists:min([Pos || {F,Pos} <- Selected, 
						     F =:= FID]),
			  [{FID,MinPos}|Acc]
		  end, [], Fs),
    State1 = {S, D#d { selected = Selected1 }},
    lists:foldl(fun(FID,Si) -> merge_fid(FID, Selected, Si) end, State1, Fs);
command($h, _Selected, Mod, State) ->
    %% sort all fields by frame id
    FIDs0Pos0 = fold_layout(
		  fun(Li, Acc, _Si) ->
			  case Li#layout.style of
			      deleted -> Acc;
			      hidden -> Acc;
			      collapsed -> Acc;
			      _ ->
				  if Mod#keymod.alt ->
					  %% sort according to original pos
					  [{Li#layout.pos0,Li#layout.id}|Acc];
				     true ->
					  %% sort according to id
					  [{0,Li#layout.id}|Acc]
				  end
			  end
		  end, [], State),
    FIDs1Pos0 = 
	if Mod#keymod.ctrl ->
		[Lj || {_SortKey,Lj} <- lists:reverse(lists:sort(FIDs0Pos0))];
	   true ->
		[Lj || {_SortKey,Lj} <- lists:sort(FIDs0Pos0)]
	end,
    FIDsPos = maps:from_list(lists:zip(FIDs1Pos0, 
				       lists:seq(1,length(FIDs1Pos0)))),
    State1 =
	each_layout(
	  fun(Li, Si) ->
		  case maps:get(Li#layout.id, FIDsPos, hidden) of
		      hidden ->
			  Si;
		      Pos ->
			  L2 = Li#layout { pos = Pos, src = Li#layout.pos },
			  L3 = position_normal(L2, Si),
			  insert_layout(L3, Si)
		  end
	  end, State),
    epxw:invalidate(),
    State1;
command($s, Selected, Mod, State) when Mod#keymod.ctrl ->
    FIDs = lists:usort([FID || {FID,_} <- Selected]),
    Bytes = 
	lists:foldr(
	  fun(FID,Acc) ->
		  Sel = lists:filter(fun({F,_}) -> F =:= FID end, Selected),
		  PosList = lists:sort([Pos || {_,Pos} <- Sel]),
		  L = lookup_layout(FID, State),
		  Format = L#layout.format,
		  FmtList = select_fmts(1, tuple_to_list(Format), [], PosList),
		  Bs = [Fmt#fmt.bits || Fmt <- FmtList],
		  %% ?verbose("SAVE FID=0x~s Bits=~w\n", [integer_to_list(FID,16), Bs]),
		  ["0x",integer_to_list(FID,16),
		   [[ %% byte index in decimal
		     [" ", integer_to_list(B div 8),  
		      %% byte mask
		      " 0x", integer_to_list((1 bsl (7-(B rem 8))), 16),
		      %% byte match value HIGH
		      " 0x", integer_to_list((1 bsl (7-(B rem 8))), 16),
		      %% byte match value LOW
		      " 0x00" ] || {B,_Len} <- G]
		    || G <- Bs],
		   "\n" | Acc]
	  end, [], FIDs),
    %% User =  os:getenv("USER"),
    Home = case os:getenv("HOME") of
	       false -> "/tmp";
	       ""    -> "/tmp";
	       H -> H
	   end,
    file:write_file(filename:join(Home, "candy.txt"), 
		    [Bytes,
		     " This line and the following lines are comments\n"
		    ]),
    State;
command($q, _Selected, _Mod, State) ->
    erlang:halt(0),
    State;

command(I, _Selected, Mod, State) when Mod#keymod.alt, I >= $1, I =< $9 ->
    bitrate_select(I-$0, State);
command(I, _Selected, Mod, State) when Mod#keymod.alt, I >= $a, I =< $e ->
    bitrate_select((I-$a)+10, State);
command(I, _Selected, Mod, State) when Mod#keymod.ctrl, I >= $1, I =< $9 ->
    datarate_select(I-$0, State);
command(I, _Selected, Mod, State) when Mod#keymod.ctrl, I >= $a, I =< $e ->
    datarate_select((I-$a)+10, State);
command(I, Selected, _Mod, State) when I >= $1, I =< $8 ->
    Fs = lists:usort([FID || {FID,_} <- Selected]),
    lists:foldl(fun(FID,Si) -> split_fid(FID, Selected, I-$0, Si) end, 
		State, Fs);
command(up, _Selected, Mod, State) when Mod#keymod.alt ->
    bitrate_prev(State);
command(down, _Selected, Mod, State) when Mod#keymod.alt ->
    bitrate_next(State);
command(up, _Selected, Mod, State) when Mod#keymod.ctrl ->
    datarate_prev(State);
command(down, _Selected, Mod, State) when Mod#keymod.ctrl ->
    datarate_next(State);
command($l, _Selected, Mod, State) when Mod#keymod.ctrl ->
    toggle_listen_only(State);
command($f, _Selected, Mod, State) when Mod#keymod.ctrl ->
    toggle_fd(State);

command(Symbol, _Selected, Mod, State) ->
    io:format("unhandled command ~p\n", [Symbol]),
    {reply,{Symbol,Mod},State}.

%% Load data display for various models (test)
load_frame_layout(prius, State) ->
    %% Break information bit 7 press=1, release=0
    load_pid_list(
      [{16#030, "Break",
	[#fmt { field=data, base=2, type=unsigned, bits=[{0,1}]}]},
       {16#025, "Steer",
	[#fmt { field=data, base=10, type=unsigned, bits=[{0,16}]}]},
       {16#0B4, "Speed",
	[#fmt { field=data, base=10, type=unsigned,
		bits=[{40,16}]}]},
       {16#244, "Veloc",
	[
	 %% speed, 0x150=10km/h, 0x300=20km 0x700=50km/h 
	 #fmt { field=data, base=10, type=signed,
		bits=[{32,16}]},
	 %% gas pedal
	 #fmt { field=data, base=10, type=unsigned,
		bits=[{56,8}]}
	]}], State);
load_frame_layout(_, State) ->
    State.

load_pid_list([{FID,Name,FormList}|List], State) ->
    State1 = load_pid(FID,Name,FormList,State),
    load_pid_list(List, State1);
load_pid_list([], State) ->
    State.

%% Note! just 11-bit format now!
load_pid(FID, Name, FormList, _State={S,D}) ->
    NameBits = iolist_to_binary(Name),
    Pos = S#s.next_pos,
    Format = 
	list_to_tuple(
	  [
	   #fmt { field=label,base=0,type={string,NameBits},bits=[]},
	   #fmt { field=id,base=16,type=unsigned,bits=[{21,11}]},
	   #fmt { field=id,base=0,type={enum,{"-","F"}},bits=[{0,1}]},
	   #fmt { field=id,base=0,type={enum,{"-","X"}},bits=[{1,1}]},
	   #fmt { field=id,base=0,type={enum,{"-","R"}},bits=[{2,1}]},
	   #fmt { field=id,base=0,type={enum,{"-","E"}},bits=[{3,1}]},
	   #fmt { field=len,base=16,type=unsigned,bits=[{2,4}]} |
	   FormList ]),
    L0 = #layout {
	    id = FID,
	    pos = Pos,
	    style = fixed, %% do not collapse
	    format = Format
	   },
    State1 = {S#s{next_pos=Pos+1}, D},
    L1 = position_layout(L0, State1),
    State2 = insert_layout(L1, State1),
    ets:insert(S#s.frame_counter, {FID, 0}),
    ets:insert(S#s.frame, #can_frame{id=FID,len=8,
					     data=(<<0,0,0,0,0,0,0,0>>)
					    }),
    redraw_fid(FID, State2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    ?verbose("Call ~p not handled\n", [_Request]),
    {reply, {error,badcall}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    ?verbose("Cast ~p not handled\n", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info(Frame=#can_frame{}, State) ->
    State1 = handle_can_frames([Frame], State),
    {noreply, State1};

handle_info({timeout,Ref, tick}, State={S,D}) when D#d.tick =:= Ref ->
    case epxw:draw(fun(Pixels,Area) ->
			   redraw_anim(Pixels,Area,State)
		   end) of
	false ->
	    {noreply, {S, D#d { tick = undefined }}};
	true ->
	    {noreply, tick_start(State)}
    end;

%%
%% Interface has changed, pick up new message
%%
handle_info({if_state_event, {ID,_IfEnt}, IfParam}, {S,D}) when 
      is_map(IfParam) ->
    ?verbose("Got if_state if=~w, param=~p\n", [ID,IfParam]),
    if ID =:= (S#s.if_can)#if_can.if_id, S#s.if_state =:= up ->
	    S1 = S#s { if_param = maps:merge(S#s.if_param, IfParam) },
	    {noreply, {S1,D}};
       true -> %% ignore
	    {noreply, {S,D}}
    end;
handle_info({if_state_event, {ID,IfParam}, IfState}, {S,D}) ->
    ?verbose("if_state_event(~w): id=~w, ifparam=~p, ifstate=~w\n", 
	     [S#s.if_state,ID,IfParam,IfState]),
    S1 = 
	if IfState =:= down, ID =:= (S#s.if_can)#if_can.if_id ->
		can_router:if_state_supervision(refresh),
		S#s { if_state = down,
		      if_param = #{},
		      if_can = #if_can{} };
	   IfState =:= up, S#s.if_state =:= down ->
		%% new interface
		LISTEN = maps:get(listen_only, IfParam),
		FD = maps:get(fd, IfParam),
		BitRate = maps:get(bitrate, IfParam),
		BitRates = maps:get(bitrates, IfParam),
		DataRate = maps:get(datarate, IfParam),
		DataRates = maps:get(datarates, IfParam),
		Mod = maps:get(mod, IfParam),
		Pid = maps:get(pid, IfParam),
		IFCan = #if_can{if_mod=Mod, if_id = ID, if_pid=Pid},
		S#s { if_state = IfState, 
		      if_can = IFCan,
		      if_param = IfParam,
		      listen_only =LISTEN,
		      fd=FD,
		      bitrate=BitRate, 
		      bitrates=sort_rates(BitRates),
		      datarate=DataRate,
		      datarates=sort_rates(DataRates) };
	   true ->
		S
	end,
    State1 = {S1,D},
    set_status(State1),
    epxw:invalidate(),
    {noreply, State1};

handle_info(_Info, State) ->
    ?verbose("Got info ~p\n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, {_S,_D}) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

sort_rates(Rates) ->
    lists:sort(Rates).

%% batch collect all frames in one go then redraw if needed
handle_can_frames(CanFrames, S) ->
    receive
	Frame = #can_frame {} ->
	    handle_can_frames([Frame|CanFrames], S)
    after 0 -> %% or a short time?
	    process_can_frames(lists:reverse(CanFrames),S,
			       false, 0, sets:new())
    end.

process_can_frames([#can_frame{id=FID}|Fs], {S,D},
		   _Redraw, RedrawCount, RedrawSet) when
      FID band ?CAN_ERR_FLAG =:= ?CAN_ERR_FLAG ->
    Err = lists:foldl(
	    fun({Bit,Flag}, Acc) ->
		    if FID band Bit =:= Bit -> [Flag|Acc];
		       true -> Acc
		    end
	    end, [], [{?CAN_ERR_TX_TIMEOUT, txtimeout},
		      {?CAN_ERR_LOSTARB,     lostarb},
		      {?CAN_ERR_CRTL,        crtl},
		      {?CAN_ERR_PROT,        prot},
		      {?CAN_ERR_TRX,         trx},
		      {?CAN_ERR_ACK,         ack},
		      {?CAN_ERR_BUSOFF,      busoff},
		      {?CAN_ERR_BUSERROR,    buserror},
		      {?CAN_ERR_RESTARTED,   restarted}]),
    %% ?verbose(" status = ~w\n", [Fs]),
    process_can_frames(Fs, {S#s { if_error = Err }, D},
		       true, RedrawCount, RedrawSet);
process_can_frames([Frame=#can_frame{id=FID}|Fs], State={S,D},
		   Redraw, RedrawCount, RedrawSet) ->
    case ets:lookup(S#s.frame, FID) of
	[Frame] -> %% no change
	    ets:update_counter(S#s.frame_counter, FID, 1),
	    process_can_frames(Fs, State, Redraw, RedrawCount, RedrawSet);
	[Frame0] ->
	    ets:insert(S#s.frame, Frame),
	    ets:update_counter(S#s.frame_counter, FID, 1),
	    case diff_frames(FID,Frame,Frame0,State) of
		[] ->
		    process_can_frames(Fs, tick_restart(State),
				       Redraw, RedrawCount, RedrawSet);
		Diff ->
		    [ ets:insert(S#s.frame_anim,{{FID,Pos},255})|| Pos <- Diff],
		    process_can_frames(Fs, tick_restart(State),
				       Redraw,
				       RedrawCount+1,
				       sets:add_element(FID,RedrawSet))
	    end;
	[] ->
	    ets:insert(S#s.frame, Frame),
	    IDFmt =
		if FID band ?CAN_EFF_FLAG =/= 0 ->
			#fmt {field=id,base=16,type=unsigned,bits=[{4,29}]};
		   true ->
			#fmt {field=id,base=16,type=unsigned,bits=[{22,11}]}
		end,
	    Format =
		if FID band ?CAN_FD_FLAG =/= 0 ->
			default_fd_format(IDFmt);
		   true ->
			default_format(IDFmt)
		end,
	    [ ets:insert(S#s.frame_anim,{{FID,Pos},255}) ||
		Pos <- lists:seq(1,tuple_size(Format))  ],
	    
	    LPos = S#s.next_pos,
	    Layout1 = #layout { id=FID, pos=LPos, pos0=LPos, format=Format },
	    Layout2 = position_layout(Layout1, State),
	    
	    State1 = {S#s{ next_pos = LPos+1}, D},
	    State2 = insert_layout(Layout2, State1),
	    
	    ets:insert(S#s.frame_counter, {FID, 1}),
	    ets:insert(S#s.frame_freq, {FID,erlang:system_time(millisecond),1,""}),
	    process_can_frames(Fs, tick_restart(State2),
			       true, RedrawCount+1, RedrawSet)
    end;
process_can_frames([], State, Redraw, _RedrawCount, RedrawSet) ->
    if Redraw ->
	    epxw:invalidate(),
	    State;
       true ->
	    case sets:size(RedrawSet) of
		0 ->
		    State;
		_Count ->
		    %% display_saved(_Count, _RedrawCount),
		    sets:fold(
		      fun(FID, Statei) ->
			      redraw_fid(FID, Statei)
		      end, State, RedrawSet)
	    end
    end.

display_saved(Count, RedrawCount) when Count < RedrawCount ->
    io:format("saved ~w redraws\n", [RedrawCount-Count]);
display_saved(_Count, _RedrawCount) ->
    ok.
    
cell_hit(Xy, Layout, State={S,D}) ->
    case fmt_from_coord(Xy, Layout) of
	false ->
	    deselect(State, []);
	{_I, Fmt} when Fmt#fmt.field =:= hide ->
	    {Layout1,State1} = remove_layout(Layout, State),
	    %% Note that Layout1 is the "delete" marked Layout
	    State2 = insert_layout(Layout1, State1),
	    {Vx,Vy,Vw,Vh} = view_rect(State2),
	    epxw:set_view_size(max(0,Vx+Vw-1), max(0,Vy+Vh-1)),
	    State2;
	    
	{_I, Fmt} when Fmt#fmt.field =:= id,
		       Layout#layout.style =:= collapsed ->
	    Layout1 = Layout#layout { style = normal },
	    update_layout(Layout, Layout1, State);

	{I, _Fmt} ->
	    Mod = epxw:keymod(),
	    if Mod#keymod.shift ->  
		    %% add to or remove from selection
		    FID = Layout#layout.id,
		    Selected = 
			case lists:member({FID,I}, D#d.selected) of
			    true -> 
				lists:delete({FID,I},D#d.selected);
			    false ->
				[{FID,I} | D#d.selected]
			end,
		    D1 = D#d { selected = Selected },
		    State1 =  {S,D1},
		    redraw_fid(FID, State1);
	       true ->
		    FID = Layout#layout.id,
		    deselect(State, [{FID,I}])
	    end
    end.

bitrate_next(State0={S,D}) ->
    I = bitrate_index(State0)-1,
    BitRates = S#s.bitrates,
    N = length(BitRates),
    J = ((I+1) rem N) + 1,
    Rate = lists:nth(J, BitRates),
    setopts(S#s.if_can, [{bitrate, Rate}]),
    [{bitrate,Rate1}] = getopts(S#s.if_can, [bitrate]),
    State = {S#s { bitrate = Rate1 }, D},
    set_status(State),
    epxw:invalidate(),
    State.

bitrate_prev(State0={S,D}) ->
    I = bitrate_index(State0)-1,
    BitRates = S#s.bitrates,
    N = length(BitRates),
    J = ((I+(N-1)) rem N) + 1,
    Rate = lists:nth(J, BitRates),
    setopts(S#s.if_can, [{bitrate, Rate}]),
    [{bitrate,Rate1}] = getopts(S#s.if_can, [bitrate]),
    State = {S#s { bitrate = Rate1 }, D},
    set_status(State),
    epxw:invalidate(),
    State.

bitrate_select(I, _State0={S,D}) ->
    BitRates = S#s.bitrates,
    N = length(BitRates),
    J = ((I-1) rem N) + 1,
    Rate = lists:nth(J, BitRates),
    ?verbose("I=~w, J=~w, Rate=~w\n", [I,J,Rate]),
    setopts(S#s.if_can, [{bitrate, Rate}]),
    [{bitrate,Rate1}] = getopts(S#s.if_can, [bitrate]),
    State = {S#s { bitrate = Rate1 }, D},
    set_status(State),
    epxw:invalidate(),
    State.

bitrate_index({S,_D}) ->
    if S#s.bitrate =:= undefined, S#s.bitrates =:= [] -> 0;
       S#s.bitrate =:= undefined, is_list(S#s.bitrates) -> 1;
       true -> index(S#s.bitrate, S#s.bitrates)
    end.

datarate_next(State0={S,D}) ->
    I = datarate_index(State0)-1,
    DataRates = S#s.datarates,
    N = length(DataRates),
    J = ((I+1) rem N) + 1,
    Rate = lists:nth(J, DataRates),
    setopts(S#s.if_can, [{datarate, Rate}]),
    [{datarate,Rate1}] = getopts(S#s.if_can, [datarate]),
    State = {S#s { datarate = Rate1 }, D},
    set_status(State),
    epxw:invalidate(),
    State.

datarate_prev(State0={S,D}) ->
    I = datarate_index(State0)-1,
    DataRates = S#s.datarates,
    N = length(DataRates),
    J = ((I+(N-1)) rem N) + 1,
    Rate = lists:nth(J, DataRates),
    setopts(S#s.if_can, [{datarate, Rate}]),
    [{datarate,Rate1}] = getopts(S#s.if_can, [datarate]),
    State = {S#s { datarate = Rate1 }, D},
    set_status(State),
    epxw:invalidate(),
    State.

datarate_select(I, _State0={S,D}) ->
    DataRates = S#s.datarates,
    N = length(DataRates),
    J = ((I-1) rem N) + 1,
    Rate = lists:nth(J, DataRates),
    ?verbose("I=~w, J=~w, Rate=~w\n", [I,J,Rate]),
    setopts(S#s.if_can, [{datarate, Rate}]),
    [{datarate,Rate1}] = getopts(S#s.if_can, [datarate]),
    State = {S#s { datarate = Rate1 }, D},
    set_status(State),
    epxw:invalidate(),
    State.

datarate_index({S,_D}) ->
    if S#s.datarate =:= undefined, S#s.datarates =:= [] -> 0;
       S#s.datarate =:= undefined, is_list(S#s.datarates) -> 1;
       true -> index(S#s.datarate, S#s.datarates)
    end.


index(E, L) -> index_(E, L, 1).
index_(E, [E|_], I) -> I;
index_(E, [_|L], I) -> index_(E, L, I+1);
index_(_E, [], _) ->  0.

%% toggle Listen-Only
toggle_listen_only({S,D}) ->
    Listen1 =
	if S#s.listen_only =:= undefined -> undefined;
	   S#s.listen_only =:= true -> false;
	   S#s.listen_only =:= false -> true
	end,
    ListenOnly =
	case S#s.if_can of
	    #if_can{if_mod=Mod,if_pid=Pid} when is_pid(Pid), 
						Listen1 =/= undefined ->
		Mod:setopts(Pid, [{listen_only, Listen1}]),
		[{listen_only,Listen2}] = Mod:getopts(Pid, [listen_only]),
		Listen2;
	    _ ->
		S#s.listen_only
	end,
    S1 = S#s{ listen_only = ListenOnly },
    State1 = {S1, D},
    set_status(State1),
    epxw:invalidate(),
    State1.    

%% toggle allow FD
toggle_fd({S,D}) ->
    FD1 =
	if S#s.fd =:= undefined -> undefined;
	   S#s.fd =:= true -> false;
	   S#s.fd =:= false -> true
	end,
    FD =
	case S#s.if_can of
	    #if_can{if_mod=Mod,if_pid=Pid} when is_pid(Pid), 
						FD1 =/= undefined ->
		Mod:setopts(Pid, [{fd, FD1}]),
		[{fd,FD2}] = Mod:getopts(Pid, [fd]),
		FD2;
	    _ ->
		S#s.fd
	end,
    S1 = S#s{ fd = FD },
    State1 = {S1, D},
    set_status(State1),
    epxw:invalidate(),
    State1.    
	    

%% deselect all and selecte the NewSelected
deselect({S,D}, NewSelected) ->
    Fs = lists:usort([FID || {FID,_} <- D#d.selected ++ NewSelected]),
    State = {S, D#d { selected = NewSelected }},
    lists:foldl(fun(FID,Si) -> redraw_fid(FID, Si) end, State, Fs).


%% Set base to 'Base' in selected cells
set_base(Selected, Base, State) ->
    lists:foldl(
      fun({FID,I}, Si) ->
	      L= #layout{format=Format} = lookup_layout(FID,Si),
	      Fmt = element(I, Format),
	      if Fmt#fmt.field =:= data ->
		      Fmt1 = Fmt#fmt { base = Base },
		      Format1 = setelement(I, Format, Fmt1),
		      L1 = L#layout{format=Format1},
		      %% FIXME: update_layout also do insert latout!
		      %% Si1 = insert_layout(L1, Si),
		      update_layout(L, L1, Si);
		 true ->
		      Si
	      end
      end, State, Selected).

select_fmts(I,[Fmt|FmtList],Acc,Sel) when  Fmt#fmt.field =:= data ->
    case lists:member(I, Sel) of
	true -> select_fmts(I+1,FmtList,[Fmt|Acc],Sel);
	false -> select_fmts(I+1,FmtList,Acc,Sel)
    end;
select_fmts(I,[_|FmtList],Acc,Sel) ->
    select_fmts(I+1,FmtList,Acc,Sel);
select_fmts(_I,[],Acc,_Sel) ->
    lists:reverse(Acc).

%% split Selected group in FID in sizes of Size bits
split_fid(FID, Selected, Size,State) ->
    Sel = lists:filter(fun({F,_}) -> F =:= FID end, Selected),
    PosList = lists:sort([Pos || {_,Pos} <- Sel]),
    L = lookup_layout(FID, State),
    Format = L#layout.format,
    FmtList1 = split_size(1, Size, tuple_to_list(Format), [], PosList),
    Format1 = list_to_tuple(FmtList1),
    L1 = L#layout { format=Format1 },
    %% State1 = insert_layout(L1, State),
    update_layout(L, L1, State).

split_half_fid(FID, Selected, State) ->
    Sel = lists:filter(fun({F,_}) -> F =:= FID end, Selected),
    PosList = lists:sort([Pos || {_,Pos} <- Sel]),
    L = lookup_layout(FID, State),
    Format = L#layout.format,
    FmtList1 = split_halfs(1, tuple_to_list(Format), [], PosList),
    Format1 = list_to_tuple(FmtList1),
    L1 = L#layout { format=Format1 },
    %% State1 = insert_layout(L1, State),
    update_layout(L, L1, State).

merge_fid(FID, Selected, State) ->
    Sel = lists:filter(fun({F,_}) -> F =:= FID end, Selected),
    PosList = lists:sort([Pos || {_,Pos} <- Sel]),
    L = lookup_layout(FID, State),
    Format = L#layout.format,
    FmtList1 = merge_fmts(1, tuple_to_list(Format), [], PosList),
    Format1 = list_to_tuple(FmtList1),
    L1 = L#layout { format=Format1 },
    State1 = insert_layout(L1, State),
    update_layout(L, L1, State1).

%% merge selected data bit fields
merge_fmts(I,[Fmt1,Fmt2|FmtList],Acc,Sel) when 
      Fmt1#fmt.field =:= data, Fmt2#fmt.field =:= data ->
    case lists:member(I, Sel) andalso lists:member(I+1,Sel) of
	true ->
	    Type = if Fmt1#fmt.type =:= signed -> signed;
		      Fmt2#fmt.type =:= signed -> signed;
		      true -> unsigned
		   end,
	    Bits = 
		case {Fmt1#fmt.bits,Fmt2#fmt.bits} of
		    {[{P1,L1}],[{P2,L2}]} when P1+L1 =:= P2 ->
			%% ?verbose("merged ~p and ~p to ~p\n",
			%% [{P1,L1},{P2,L2},{P1,L1+L2}]),
			[{P1,L1+L2}];
		    {Bits1,Bits2} ->
			Bs = Bits1++Bits2,
			io:format("warning: Bits appended ~w\n", [Bs]),
			Bs
		end,
	    Fmt3 = Fmt1#fmt { type=Type, bits=Bits },
	    merge_fmts(I+1, [Fmt3|FmtList], Acc, Sel);
	false ->
	    merge_fmts(I+1, [Fmt2|FmtList], [Fmt1|Acc], Sel)
    end;
merge_fmts(I, [Fmt|FmtList], Acc, Sel) ->
    merge_fmts(I+1, FmtList, [Fmt|Acc], Sel);
merge_fmts(_I, [], Acc, _Sel) ->
    lists:reverse(Acc).

%% split selected data bit fields in halfs
split_halfs(I,[Fmt|FmtList],Acc,Sel) when  Fmt#fmt.field =:= data ->
    case lists:member(I, Sel) of
	true ->
	    case Fmt#fmt.bits of
		[{Pos,Len}] when Len > 1 ->
		    Chunk = (Len+1) div 2,
		    Acc1 = make_bit_chunks(Fmt, Pos, Chunk, Len, Acc),
		    split_halfs(I+1,FmtList,Acc1, Sel);
		_ -> %% FIXME a bit more work
		    ?debug("fixme: split bits ~w\n", [Fmt#fmt.bits]),
		    split_halfs(I+1,FmtList,[Fmt|Acc],Sel)
	    end;
	false ->
	    split_halfs(I+1, FmtList, [Fmt|Acc], Sel)
    end;
split_halfs(I, [Fmt|FmtList], Acc, Sel) ->
    split_halfs(I+1, FmtList, [Fmt|Acc], Sel);
split_halfs(_I, [], Acc, _Sel) ->
    lists:reverse(Acc).

%% split selected data bit fields in size chunks
split_size(I,Size,[Fmt|FmtList],Acc,Sel) when Fmt#fmt.field =:= data ->
    case lists:member(I, Sel) of
	true ->
	    case Fmt#fmt.bits of
		[{Pos,Len}] when Len > 1 ->
		    Chunk = if Size =:= half -> (Len+1) div 2;
			       Size =< Len -> Size;
			       true -> Len
			    end,
		    %% {Pos,L1}, {Pos+L1,L1} {Pos+2*L1,L1} ... {Pos+N*L1,L2}
		    Acc1 = make_bit_chunks(Fmt, Pos, Chunk, Len, Acc),
		    split_size(I+1,Size,FmtList,Acc1, Sel);
		_ -> %% FIXME a bit more work
		    ?debug("fixme: split bits ~w\n", [Fmt#fmt.bits]),
		    split_size(I+1,Size,FmtList,[Fmt|Acc],Sel)
	    end;
	false ->
	    split_size(I+1, Size, FmtList, [Fmt|Acc], Sel)
    end;
split_size(I, Size, [Fmt|FmtList], Acc, Sel) ->
    split_size(I+1, Size, FmtList, [Fmt|Acc], Sel);
split_size(_I, _Size, [], Acc, _Sel) ->
    lists:reverse(Acc).

make_bit_chunks(Fmt, Pos, Chunk, Remain, Acc) when Chunk =< Remain ->
    make_bit_chunks(Fmt,Pos+Chunk, Chunk, Remain-Chunk,
		   [Fmt#fmt{ bits=[{Pos,Chunk}]}|Acc]);
make_bit_chunks(_Fmt, _Pos, _Chunk, 0, Acc) ->
    Acc;
make_bit_chunks(Fmt, Pos, _Chunk, Remain, Acc) ->
    [Fmt#fmt{ bits=[{Pos,Remain}]}|Acc].

tick_start(_State={S,D}) ->
    {S, D#d { tick = erlang:start_timer(100, self(), tick)}}.

tick_restart(State={_S,D}) when D#d.tick =:= undefined ->
    tick_start(State);
tick_restart(State) ->
    State.

tick_stop(State={_S,D}) when D#d.tick =:= undefined ->
    State;
tick_stop(_State={S,D}) ->
    erlang:cancel_time(D#d.tick),
    {S, D#d { tick = undefined }}.
    
default_format(IDFmt) ->
    {
     #fmt { field=hide, type=undefined },
     IDFmt,
     #fmt { field=frequency,base=10,type={float,5,1},bits=[]},
     #fmt { field=id,base=0,type={enum,{"-","F"}},bits=[{0,1}]},
     #fmt { field=id,base=0,type={enum,{"-","X"}},bits=[{1,1}]},
     #fmt { field=id,base=0,type={enum,{"-","R"}},bits=[{2,1}]},
     #fmt { field=id,base=0,type={enum,{"-","E"}},bits=[{3,1}]},
     #fmt { field=len,base=16,type=unsigned,bits=[{2,4}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{0,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{8,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{16,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{24,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{32,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{40,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{48,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{56,8}]}
    }.

default_fd_format(IDFmt) ->
    {
     #fmt { field=hide, type=undefined },
     IDFmt,
     #fmt { field=frequency,base=10,type={float,5,1},bits=[]},
     #fmt { field=id,base=0,type={enum,{"-","F"}},bits=[{0,1}]},
     #fmt { field=id,base=0,type={enum,{"-","X"}},bits=[{1,1}]},
     #fmt { field=id,base=0,type={enum,{"-","R"}},bits=[{2,1}]},
     #fmt { field=id,base=0,type={enum,{"-","E"}},bits=[{3,1}]},
     #fmt { field=len,base=16,type=unsigned,bits=[{0,7}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{0,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{8,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{16,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{24,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{32,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{40,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{48,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{56,8}]},
     %% FD data
     #fmt { field=data, base=c, type=unsigned, bits=[{64,32}], ci=2},
     #fmt { field=data, base=c, type=unsigned, bits=[{96,32}], ci=3},
     #fmt { field=data, base=c, type=unsigned, bits=[{128,32}], ci=4},
     #fmt { field=data, base=c, type=unsigned, bits=[{160,32}], ci=5},
     #fmt { field=data, base=c, type=unsigned, bits=[{192,32}], ci=6},
     #fmt { field=data, base=c, type=unsigned, bits=[{224,32}], ci=7},

     #fmt { field=data, base=c, type=unsigned, bits=[{256,64}], ci=8},
     #fmt { field=data, base=c, type=unsigned, bits=[{320,64}], ci=9},
     #fmt { field=data, base=c, type=unsigned, bits=[{384,64}], ci=10},
     #fmt { field=data, base=c, type=unsigned, bits=[{448,64}], ci=11}
    }.

%% bits_int8(Pos)  -> [{Pos,8}].
%% bits_int16(Pos) -> [{Pos,16}].
%% bits_int32(Pos) -> [{Pos,32}].

%% bits_int16_LE(Pos) -> [{Pos+8,8},{Pos,8}].
%% bits_int32_LE(Pos) -> [{Pos+24,8},{Pos+16},{Pos+8},{Pos,8}].

diff_frames(FID, New, Old, State) ->
    #layout{format=Format} = Layout = lookup_layout(FID, State),
    if Layout#layout.style =:= deleted ->
	    [];
       true ->
	    case diff_frames_(1,Format,FID,New,Old,[],State) of
		[] -> [];
		_Diff when Layout#layout.style =:= collapsed -> 
		    %% flash only the ID field
		    [?ID_FMT_POSITION];
		Diff -> Diff
	    end
    end.

diff_frames_(Pos,Format,FID,New,Old,Acc,State) when Pos =< tuple_size(Format) ->
    Fmt = element(Pos, Format),
    BitsDataNew = get_bits(Fmt,New),
    BitsDataOld = get_bits(Fmt,Old),
    if BitsDataNew =:= BitsDataOld ->
	    diff_frames_(Pos+1,Format,FID,New,Old,Acc,State);
       true ->
	    diff_frames_(Pos+1,Format,FID,New,Old,[Pos|Acc],State)
    end;
diff_frames_(_Pos,_Format,_FID,_New,_Old,Acc,_State) ->
    Acc.

redraw_anim(Pixels,Area,State={S,_}) ->
    case ets:first(S#s.frame_anim) of
	'$end_of_table' ->
	    false;
	First ->
	    {Remove,Update} = redraw_anim_(Pixels,Area,First, [], [], State),
	    ets:delete(S#s.frame_anim, Remove),
	    lists:foreach(
	      fun(Key) ->
		      %% FIXME: use update counter
		      case ets:lookup(S#s.frame_anim, Key) of
			  [] -> ok;
			  [{_,Val}] ->
			      Val1 = max(0, Val-10),
			      ets:insert(S#s.frame_anim, {Key,Val1})
		      end
	      end, Update),
	    Update =/= []
    end.

redraw_anim_(Pixels,Area,Key={_FID,0}, Remove, Update, State={S,_}) ->
    %% redraw the entire layout using position interpolation
    Next = ets:next(S#s.frame_anim, Key),
    redraw_anim_(Pixels,Area,Next, Remove, Update, State);
redraw_anim_(Pixels,Area,Key={FID,Pos}, Remove, Update, State={S,_}) ->
    [Frame] = ets:lookup(S#s.frame, FID),
    Next = ets:next(S#s.frame_anim, Key),
    case redraw_pos(Pixels,Area,FID,Pos,Frame,State) of
	true -> %% remove
	    redraw_anim_(Pixels,Area,Next, [Key|Remove], Update, State);
	false ->
	    redraw_anim_(Pixels,Area,Next, Remove, [Key|Update], State)
    end;
redraw_anim_(_Pixels,_Area,'$end_of_table', Remove, Update, _State) ->
    {Remove, Update}.

%% +-----------+---------------+----------------+
%% | Link: up  | BitRate: 250k | Status: busoff |
%% +-----------+---------------+----------------+
set_status(_State={S,_D}) ->
    LinkState = case S#s.if_state of
		    up -> "Link: up";
		    down -> "Link: down"
		end,
    LinkBitRate =
	case S#s.bitrate of
	    error ->   "BitRate: error";
	    undefined ->   "BitRate: undef";
	    BitRate -> "BitRate: " ++ integer_to_list(BitRate div 1000)++"k"
	end,
    LinkFlags =
	"Flags: " ++ 
	lists:join(",", 
		   lists:append(case S#s.listen_only of 
				    true -> ["LISTEN"];
				    _ -> []
				end,
				case S#s.fd of 
				    true -> ["FD"];
				    _ -> []
				end)),
    LinkError =
	case S#s.if_error of 
	    [] -> "State: ok";
	    Es -> "State: "++ format_error(Es)
	end,
    Status = io_lib:format("~-12s ~-16s ~-16s ~-20s", 
			   [LinkState,LinkFlags,LinkBitRate,LinkError]),
    epxw:set_status_text(Status),

    WindowTitle = case maps:get(device_name, S#s.if_param, "") of
		      "/dev/serial/by-id/usb-LAWICEL_CANUSB_" ++ _ ->
			  "Candy@CANUSB";
		      "" -> "Candy";
		      DeviceName -> "Candy@"++DeviceName
		  end,
    Window = epxw:window(),
    epx:window_adjust(Window, [{name, WindowTitle}]).

format_error(ErrorList) ->
    string:join([atom_to_list(E) || E <- ErrorList], ",").

%% redraw all Layouts 
redraw_all(Pixels, _Area, State) ->
    each_layout(fun(Layout, State1) ->
			%% FIXME: check intersection with _Area
			redraw_layout_(Pixels, Layout, State1)
		end, State).

redraw_(Pixels, _Area, FID, State) ->
    Layout = lookup_layout(FID, State),
    %% Fixme check intersection with Area
    redraw_layout_(Pixels, Layout, State).

redraw_fid(FID, State={_S,_D}) ->
    epxw:draw(
      fun(Pixels, _Area) ->
	      Layout = lookup_layout(FID, State),
	      %% Fixme check intersection with Area
	      redraw_layout_(Pixels, Layout, State)
      end).


each_layout(Fun, State={S,_D}) ->
    Tab = S#s.frame_layout,
    each_layout_(Fun, Tab, ets:first(Tab), State).

each_layout_(_Fun, _Tab, '$end_of_table', State) ->
    State;
each_layout_(Fun, Tab, FID, State) ->
    Layout = lookup_layout(FID, State),
    State1 = Fun(Layout, State),
    each_layout_(Fun, Tab, ets:next(Tab, FID), State1).

fold_layout(Fun, Acc, State={S,_D}) ->
    Tab = S#s.frame_layout,
    fold_layout_(Fun, Acc, Tab, ets:first(Tab), State).

fold_layout_(_Fun, Acc, _Tab, '$end_of_table', _State) ->
    Acc;
fold_layout_(Fun, Acc, Tab, FID, State) ->
    Layout = lookup_layout(FID, State),
    Acc1 = Fun(Layout, Acc, State),
    fold_layout_(Fun, Acc1, Tab, ets:next(Tab, FID), State).
    
redraw_layout_(Pixels, Layout, State={S,_D}) ->
    #layout{ id=FID, color=Color,format=FmtTuple} = Layout,
    %% Count = ets:lookup_element(S#s.frame_counter, FID, 2),
    draw_layout_background(Pixels, Layout),
    [Frame] = ets:lookup(S#s.frame, FID),
    case Layout#layout.style of
	deleted ->
	    State;
	collapsed ->
	    _Remove = redraw_cell(Pixels,
				  Layout#layout.x,Layout#layout.y,
				  FID,?ID_FMT_POSITION,Color,
				  element(?ID_FMT_POSITION,FmtTuple),
				  Frame,State),
	    State;
	_ ->
	    redraw_cells(Pixels,
			 Layout#layout.x,Layout#layout.y,
			 FID,1,Color,FmtTuple,Frame,State)
    end.

redraw_cells(Pixels,Lx,Ly,FID,Pos,Color,FmtTuple,Frame,State) when 
      Pos =< tuple_size(FmtTuple) ->
    Fmt = element(Pos, FmtTuple),
    _Remove = redraw_cell(Pixels, Lx,Ly,FID,Pos,Color,Fmt,Frame,State),
    redraw_cells(Pixels,Lx,Ly,FID,Pos+1,Color,FmtTuple,Frame,State);
redraw_cells(_Pixels,_Lx,_Ly,_FID,_Pos,_Color,_FmtTuple,_Frame,State) ->
    State.

redraw_pos(Pixels,Area,FID,Pos,Frame,State) ->
    Layout = lookup_layout(FID, State),
    %% FIXME: filter using Area!
    #layout{ x=Lx, y=Ly, color=Color, format=FmtTuple} = Layout,
    case Layout#layout.style of
	collapsed ->
	    redraw_pos(Pixels,Lx,Ly,FID,?ID_FMT_POSITION,Color,FmtTuple,Frame,State);
	deleted ->
	    true;
	_ ->
	    redraw_pos(Pixels,Lx,Ly,FID,Pos,Color,FmtTuple,Frame,State)
    end.

redraw_pos(Pixels,Lx,Ly,FID,Pos,Color,FmtTuple,Frame,State) ->
    if Pos > tuple_size(FmtTuple) ->
	    true;
       true ->
	    Fmt = element(Pos,FmtTuple),
	    redraw_cell(Pixels,Lx,Ly,FID,Pos,Color,Fmt,Frame,State)
    end.

redraw_cell(Pixels,Lx,Ly,FID,Pos,_LayoutColor,Fmt,Frame,State={S,D}) ->
    #fmt {dx=Dx,dy=Dy,width=W,height=H,ci=Ci} = Fmt,
    Color = cell_color(Ci),
    X = Lx+Dx, Y = Ly+Dy,
    Anim = get_anim(FID,Pos,State),
    {Remove,TextColor} = highlight(Pixels,Anim,Color,{X,Y,W,H},State),
    BitsData = get_bits(Fmt, Frame),
    %% draw shape
    if Fmt#fmt.field =/= hide ->
	    epx_gc:set_fill_style(none),
	    epx_gc:set_foreground_color(Color#color.border),
	    epx_gc:set_border_color(Color#color.border),
	    epx_gc:set_border_style(inside),
	    case lists:member({FID,Pos}, D#d.selected) of
		true -> epx_gc:set_border_width(2);
		_ -> epx_gc:set_border_width(0)
	    end,
	    epx:draw_rectangle(Pixels, {X,Y,W,H}),
	    epxw:invalidate({X,Y,W,H}),
	    epx_gc:set_border_width(0);
       true ->
	    ok
    end,

    %% draw base indicator, only for data fields
    if Fmt#fmt.field =:= data ->
	    case Fmt#fmt.base of
		2  ->
		    epx:pixmap_put_pixels(Pixels,
					  X+1,Y+1,6,7,argb,bin_icon(),blend),
		    epx:draw_rectangle(Pixels,{X,Y,8,9});
		8  ->
		    epx:pixmap_put_pixels(Pixels,
					  X+1,Y+1,6,7,argb,oct_icon(),blend),
		    epx:draw_rectangle(Pixels,{X,Y,8,9});
		16 ->
		    epx:pixmap_put_pixels(Pixels,
					  X+1,Y+1,6,7,argb,hex_icon(),blend),
		    epx:draw_rectangle(Pixels,{X,Y,8,9});
		10 ->
		    epx:pixmap_put_pixels(Pixels,
					  X+1,Y+1,6,7,argb,dec_icon(),blend),
		    epx:draw_rectangle(Pixels,{X,Y,8,9});
		0  ->
		    false;
		c ->
		    false
	    end;
       true ->
	    false
    end,
    epx_gc:set_font(S#s.font),
    epx_gc:set_foreground_color(TextColor),
    case Fmt#fmt.field of
	frequency -> %% frequency in K frames/s
	    Time1 = erlang:system_time(millisecond),
	    [{_,Time0,Count0,String0}] = 
		ets:lookup(S#s.frame_freq, FID),
	    Td = Time1 - Time0,
	    String =
		if Td > 1000 ->
			[{_,Count1}] = 
			    ets:lookup(S#s.frame_counter,FID),
			N = Count1 - Count0,
			Ffmt = case Fmt#fmt.type of
				   {float,Fw,Fp} -> [$~,(Fw+$0),$.,(Fp+$0),$f];
				   {float,Fp} -> [$~,$.,(Fp+$0),$f]
			       end,
			String1 = lists:flatten(io_lib:format(Ffmt, [(1000*N)/Td])),
			ets:insert(S#s.frame_freq,
				   {FID,Time1,Count1,String1}),
			String1;
		   true ->
			String0
		end,
	    GA = glyph_ascent(S),
	    Ya = Y+1+GA,
	    Offs = 2,
	    epx:draw_string(Pixels,X+Offs,Ya,String);
	hide ->
	    epx:pixmap_copy_area(S#s.hide, 
				 Pixels,
				 0, 0, X+2, Y+2, 16, 16, [blend]);
	_ ->
	    String = fmt_bits(Fmt#fmt.type, Fmt#fmt.base, BitsData),
	    GA = glyph_ascent(S),
	    Ya = Y+1+GA,
	    Offs = if Fmt#fmt.base > 0, Fmt#fmt.field =:= data -> 6+2;
		      true -> 2
		   end,
	    epx:draw_string(Pixels, X+Offs, Ya, String)
    end,
    Remove.

-define(COLOR(H),
	       #color {
		  background  = ?LAYOUT_BACKGROUND_COLOR,
		  foreground  = ?TEXT_COLOR,               %% text color
		  border      = ?FRAME_BORDER_COLOR,       %% frame border color
		  background1  = H,
		  foreground1  = ?TEXT_HIGHLIGHT_COLOR
		 }).

cell_color(I) ->
    element(I, 
	    { ?COLOR(?HIGHLIGHT_COLOR1),
	      ?COLOR(?HIGHLIGHT_COLOR2),
	      ?COLOR(?HIGHLIGHT_COLOR3),
	      ?COLOR(?HIGHLIGHT_COLOR4),
	      ?COLOR(?HIGHLIGHT_COLOR5),
	      ?COLOR(?HIGHLIGHT_COLOR6),
	      ?COLOR(?HIGHLIGHT_COLOR7),
	      ?COLOR(?HIGHLIGHT_COLOR8),
	      ?COLOR(?HIGHLIGHT_COLOR9),
	      ?COLOR(?HIGHLIGHT_COLOR10),
	      ?COLOR(?HIGHLIGHT_COLOR11),
	      ?COLOR(?HIGHLIGHT_COLOR12),
	      ?COLOR(?HIGHLIGHT_COLOR13)  }).

%% "delete" the layout by drawing layout background color
draw_layout_background(Pixels, Layout) ->
    Color = Layout#layout.color,
    draw_layout_rectangle(Pixels, Layout, Color#color.background,
			  Layout#layout.width).

%% prepare layout by painting the layout background color
clear_layout_background(Pixels, Layout, State={S,_D}) ->
    Color = ?LAYOUT_BACKGROUND_COLOR,
    draw_layout_rectangle(Pixels, Layout, Color, epxw:width()).

draw_layout_rectangle(Pixels, Layout, Color, Width) ->
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(Color),
    {X,Y} = {Layout#layout.x, Layout#layout.y},
    Rect = {X,Y,Width,Layout#layout.height},
    epx:draw_rectangle(Pixels, Rect).


glyph_width(S) -> S#s.glyph_width.
%%glyph_height(S) -> S#s.glyph_height.
glyph_ascent(S) -> S#s.glyph_ascent.

get_anim(FID,Pos,{S,_}) ->
    case ets:lookup(S#s.frame_anim, {FID,Pos}) of
	[] -> -1;
	[{_,Val}] -> Val
    end.

%% draw hightlight background return text color and flag to signal remove
highlight(_Pixels,-1, Color, _Rect, _State) ->
    {true, Color#color.foreground};
highlight(Pixels,0, Color, Rect, _State) ->
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(Color#color.background),
    epx:draw_rectangle(Pixels,Rect),
    {true,  Color#color.foreground};
highlight(Pixels,Anim, Color, Rect, _State) ->
    B1 = Color#color.background1,
    B0 = Color#color.background,
    B  = blend(Anim, B1, B0),
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(B),
    epx:draw_rectangle(Pixels,Rect),
    T1 = Color#color.foreground1,
    T0 = Color#color.foreground,
    T  = blend(Anim, T1, T0),
    {false, T}.


blend(Val, {Rh,Gh,Bh}, {Rb,Gb,Bb}) ->
    F1 = Val/255,
    F0 = 1-F1,
    R = trunc(F1*Rh + F0*Rb),
    G = trunc(F1*Gh + F0*Gb),
    B = trunc(F1*Bh + F0*Bb),
    {R,G,B};
blend(Val, {Ah,Rh,Gh,Bh}, {Ab,Rb,Gb,Bb}) ->
    F1 = Val/255,
    F0 = 1-F1,
    A = trunc(F1*Ah + F0*Ab),
    R = trunc(F1*Rh + F0*Rb),
    G = trunc(F1*Gh + F0*Gb),
    B = trunc(F1*Bh + F0*Bb),
    {A,R,G,B}.

interp_coord(Val, {X1,Y1}, {X0,Y0}) ->
    F1 = Val/255,
    F0 = 1-F1,
    X = trunc(F1*X1 + F0*X0),
    Y = trunc(F1*Y1 + F0*Y0),
    {X,Y}.

get_bits(#fmt { type={string,NameBits} }, _Frame) ->
    NameBits;
get_bits(#fmt { field=frequency }, _Frame) -> 
    <<>>;
get_bits(#fmt { field=hide }, _Frame) -> 
    <<>>;
get_bits(Fmt, Frame) ->
    Data = case Fmt#fmt.field of
	       data -> extend_bits(Frame#can_frame.data, 64);
	       id   -> <<(Frame#can_frame.id):33>>;  %% 33 bits!!! FD support
	       len  -> <<(Frame#can_frame.len):7>>
	   end,
    collect_bits(Fmt#fmt.bits, Data).

extend_bits(Data, MinSize) when bit_size(Data) >= MinSize ->
    Data;
extend_bits(Data, MinSize) ->
    Size = bit_size(Data),
    Pad = MinSize - Size,
    <<Data/bitstring, 0:Pad>>.

layout_rect(#layout { x=X0, y=Y0, width=W0, height=H0 }) ->
    {X0,Y0,W0,H0}.

invalidate_layout(Layout, State) ->
    #layout { x=X0, y=Y0, width=W0, height=H0 } = Layout,
    epxw:invalidate({X0,Y0,W0,H0}),
    State.

%% Layout is to be "delete", 
%% reposition layouts below the Layout, update next_pos
%% update view_height / view_width

remove_layout(Layout, State) ->
    State1 =
	each_layout(fun(L1, Si) ->
			    Pi = L1#layout.pos,
			    if Pi > Layout#layout.pos ->
				    L2 = L1#layout { pos = Pi-1 },
				    L3 = position_normal(L2, Si),
				    insert_layout(L3, Si);
			       true ->
				    Si
			    end
		    end, State),
    {S1,D1} = State1,
    S2 = S1#s{ next_pos = S1#s.next_pos-1 },
    {Layout#layout { style = deleted,
		     pos=0, x=0,y=0,width=0,height=0
		   }, {S2,D1}}.

%% calculate new view rect
view_rect(State) ->
    fold_layout(fun(L1, Ri, _Si) when L1#layout.style =:= normal;
				      L1#layout.style =:= fixed ->
			R = {L1#layout.x,L1#layout.y,
			     L1#layout.width,L1#layout.height},
			epx_rect:union(Ri, R);
		   (_L1, Ri, _Si) ->
			Ri
		end, {0,0,0,0}, State).

%% recalulate Layout
position_layout(Layout, State) ->
    case Layout#layout.style of
	normal    -> position_normal(Layout,State);
	fixed     -> position_normal(Layout,State);
	collapsed -> position_collapsed(Layout,State);
	deleted   -> position_deleted(Layout,State)
    end.

position_deleted(Layout, _State) ->
    Layout#layout { x=0,y=0,width=0,height=0 }.

position_to_coord(Pos, {S,_D}) when is_integer(Pos), Pos > 0 ->
    {0, (Pos-1)*(S#s.row_height+S#s.row_pad)}.

position_normal(Layout, State={S,_D}) ->
    {X,Y} = position_to_coord(Layout#layout.pos, State),
    {Dx,FmtTuple} = 
	position_cells(0, 1, Layout#layout.format, [], State),
    Width = Dx-1,
    Height = S#s.row_height,
    Layout#layout { x=X,y=Y,width=Width,height=Height,format=FmtTuple }.

position_collapsed(Layout, _State={S,_D}) ->
    J = Layout#layout.pos-1,
    Fmt = element(?ID_FMT_POSITION, Layout#layout.format), %% FIXME!
    Row = 1,  %% calculate row from bottom left to right
    Height = S#s.row_height,
    Num    = num_glyphs(Fmt),
    GW = glyph_width(S),
    Wide   = 8*GW + 4,
    Width  = Num*GW + 4,
    X = J*Wide,
    Y = epxw:height() - Row*Height,
    Fmt1 = Fmt#fmt { dx=0, dy=0, width=Width, height=Height },
    FmtTuple = setelement(?ID_FMT_POSITION, Layout#layout.format, Fmt1),
    Layout#layout { x=X,y=Y,width=Wide+2,height=Height,format=FmtTuple}.

position_cells(Dx,I,FmtTuple,Acc,State={S,_D}) when I =< tuple_size(FmtTuple) ->
    Fmt = element(I, FmtTuple),
    Num = num_glyphs(Fmt),
    GW = glyph_width(S),
    Width0 = Num*GW,
    Wind = if Fmt#fmt.field =:= data -> 6; %% what was this?
	      true -> 0
	   end,
    Width = Wind + Width0 + 4,
    Height = S#s.row_height,
    Fmt1 = Fmt#fmt { dx=Dx, dy=0, width=Width, height=Height },
    position_cells(Dx+Width+2, I+1, FmtTuple, [Fmt1|Acc], State);
position_cells(Dx, I, _FmtTuple, Acc, _State) ->
    FmtTuple1 = list_to_tuple(lists:reverse(Acc)),
    if I =:= 1 -> {0,FmtTuple1};
       true -> {Dx-2,FmtTuple1}
    end.

num_glyphs(Fmt) ->
    case Fmt#fmt.type of
	undefined ->
	    2;
	unsigned ->
	    Size = lists:sum([Len || {_Pos,Len} <- Fmt#fmt.bits]),
	    N = number_of_digits(Fmt#fmt.base, Size),
	    N;
	signed ->
	    Size = lists:sum([Len || {_Pos,Len} <- Fmt#fmt.bits]),
	    N = number_of_digits(Fmt#fmt.base, Size),
	    %% one extra char for -/+ sign
	    (N+1);
	{enum,Names} when is_tuple(Names) ->
	    N = lists:max([length(Name)||Name <-tuple_to_list(Names)]),
	    N;
	{string,String} -> %% fixed string
	    byte_size(String);
	{float,W,_P} -> W+1
    end.

fmt_bits(unsigned,Base,BitsData) ->
    Size = bit_size(BitsData),
    <<Number:Size/unsigned>> = BitsData,
    fmt_num(Base, Size, Number);
fmt_bits({enum,Es},_Base,BitsData) ->
    Size = bit_size(BitsData),
    <<Number:Size/unsigned>> = BitsData,
    element((Number+1),Es);
fmt_bits(signed,Base,BitsData) ->
    Size = bit_size(BitsData),
    <<Number:Size/signed>> = BitsData,
    fmt_num(Base, Size, Number);
fmt_bits({string,_String},_Base,BitsData) ->
    binary_to_list(BitsData).


fmt_num(c, Size, _Number) ->
    N = max(1, (Size+15) div 16),
    lists:duplicate(N, $\s);
fmt_num(Base, Size, Number) ->
    tl(integer_to_list(hi_digit(Base, Size) + Number, Base)).

hi_digit(2, Size) ->
    N = number_of_digits(2, Size),
    pow(2, N);
hi_digit(8, Size) ->
    N = number_of_digits(8, Size),
    pow(8, N);
hi_digit(16, Size) ->
    N = number_of_digits(16, Size),
    pow(16, N);
hi_digit(10, Size) ->
    N = number_of_digits(10, Size),
    pow(10, N);
hi_digit(c, Size) ->
    N = max(1, (Size+15) div 16),
    pow(8, N).

number_of_digits(2,  Size) -> Size;
number_of_digits(8,  Size) -> ((Size+2) div 3);
number_of_digits(16, Size) -> ((Size+3) div 4);
number_of_digits(10, Size) -> trunc(math:log10((1 bsl Size)-1)) + 1;
number_of_digits(c,  Size) -> max(1, (Size+15) div 16).
    

pow(A, B) when is_integer(A),is_integer(B), B >= 0 ->
    if A =:= 0 -> 0;
       A =:= 1 -> 1;
       A =:= -1 -> (1 - 2*(B band 1));
       true -> pow_(A, B, 1)
    end.

pow_(A, 1, Prod) -> A*Prod;
pow_(_A, 0, Prod) -> Prod;
pow_(A, B, Prod)  ->
    B1 = B bsr 1,
    A1 = A*A,
    if B - B1 == B1 ->
	    pow_(A1, B1, Prod);
       true ->
	    pow_(A1, B1, (A*Prod))
    end.


%% collect_bits
%% picks collects bits in the order given from 
%% a list of [{P,L}], with the position P with length L.
%%
%% example read reversed 32 bits in groups of 8
%% [{24,8},{16,8},{8,8},{0,8}]
%% 
collect_bits([{P,L}|Ps], Data) when is_bitstring(Data) ->
    case Data of
	<<_:P, Bits:L/bitstring, _/bitstring>> ->
	    collect_bits_(Ps, Data, Bits);
	_ ->
	    collect_bits_(Ps, Data, <<0:L>>)
    end.

collect_bits_([{P,L}|Ps], Data, Acc) ->
    case Data of
	<<_:P, Bits:L/bitstring, _/bitstring>> ->
	    collect_bits_(Ps, Data, <<Acc/bitstring, Bits/bitstring>>);
	_ ->
	    collect_bits_(Ps, Data, <<Acc/bitstring, 0:L>>)
    end;
collect_bits_([], _Data, Acc) ->
    Acc.

lookup_layout(FID, _State={S,_D}) ->
    [L] = ets:lookup(S#s.frame_layout, FID),  %% lookup element
    L.

update_layout(OldLayout, NewLayout, State={_S,_D}) ->
    Pixels = epxw:pixels(),
    clear_layout_background(Pixels, OldLayout, State),
    Layout = position_layout(NewLayout, State),
    State1 = insert_layout(Layout, State),
    State2 = redraw_layout_(Pixels, Layout, State1),
    if OldLayout#layout.x =/= Layout#layout.x;
       OldLayout#layout.y =/= Layout#layout.y;
       OldLayout#layout.width =/= Layout#layout.width;
       OldLayout#layout.height =/= Layout#layout.height ->
	    %% if layout changed repaint old area
	    invalidate_layout(OldLayout, State2);
       true ->
	    ok
    end,
    invalidate_layout(Layout, State2).


insert_layout(Layout, State={S,_D}) ->
    ets:insert(S#s.frame_layout, Layout),
    epxw:set_view_size(Layout#layout.x+Layout#layout.width,
		       Layout#layout.y+Layout#layout.height),
    State.


layout_from_coord(XY, _State={S,_D}) ->
    Tab = S#s.frame_layout,
    Key = ets:first(Tab),
    layout_from_coord_(XY, Tab, Key).

layout_from_coord_(_XY, _Tab, '$end_of_table') ->
    false;
layout_from_coord_(XY, Tab, FID) ->
    case ets:lookup(Tab, FID) of
	[] ->
	    layout_from_coord_(XY, Tab, ets:next(Tab, FID));
	[Layout=#layout{x=X1,y=Y1,width=W,height=H}] ->
	    {X,Y} = XY,
	    if X >= X1, Y >= Y1, X < X1+W, Y < Y1+H ->
		    Layout;
	       true ->
		    layout_from_coord_(XY, Tab, ets:next(Tab, FID))
	    end
    end.

fmt_from_coord(Xy, Layout) ->
    fmt_from_coord_(Xy, 1, Layout#layout.x,Layout#layout.y, 
		    Layout#layout.format).

fmt_from_coord_(Xy={X,Y}, I, Lx, Ly, FmtTuple) when I =< tuple_size(FmtTuple) ->
    case element(I, FmtTuple) of
	F = #fmt{dx=Dx,dy=Dy,width=W,height=H} when
	      X >= (Lx+Dx), Y >= (Ly+Dy), X < Lx+Dx+W, Y < Ly+Dy+H ->    
	    {I, F};
	_ ->
	    fmt_from_coord_(Xy, I+1, Lx, Ly, FmtTuple)
    end;
fmt_from_coord_(_Xy, _I, _Lx, _Ly, _FmtTuple) ->
    false.

%%
%% ICONS
%%
-define(T, 0,0,0,0).         %% transparent
-define(R, 255,255,0,0).     %% red
-define(G, 255,0,255,0).     %% green
-define(B, 255,0,0,255).     %% blue
-define(C, 255,255,255,0).   %% cyan
-define(X, 255,0,0,0).       %% black
-define(W, 255,255,255,255). %% white

bin_icon() ->
    <<
      ?T,?T,?T,?T,?T,?T,
      ?T,?X,?X,?X,?X,?T,
      ?T,?T,?T,?T,?X,?T,
      ?T,?X,?X,?X,?X,?T,
      ?T,?X,?T,?T,?T,?T,
      ?T,?X,?X,?X,?X,?T,
      ?T,?T,?T,?T,?T,?T
    >>.

oct_icon() ->
    <<
      ?T,?T,?T,?T,?T,?T,
      ?T,?X,?X,?X,?X,?T,
      ?T,?X,?T,?T,?X,?T,
      ?T,?X,?X,?X,?X,?T,
      ?T,?X,?T,?T,?X,?T,
      ?T,?X,?X,?X,?X,?T,
      ?T,?T,?T,?T,?T,?T
    >>.

hex_icon() ->
    <<
      ?T,?T,?T,?T,?T,?T,
      ?T,?X,?T,?T,?X,?T,
      ?T,?X,?T,?T,?X,?T,
      ?T,?T,?X,?X,?T,?T,
      ?T,?X,?T,?T,?X,?T,
      ?T,?X,?T,?T,?X,?T,
      ?T,?T,?T,?T,?T,?T
    >>.

dec_icon() ->
    <<
      ?T,?T,?T,?T,?T,?T,
      ?T,?X,?X,?X,?T,?T,
      ?T,?X,?T,?T,?X,?T,
      ?T,?X,?T,?T,?X,?T,
      ?T,?X,?T,?T,?X,?T,
      ?T,?X,?X,?X,?T,?T,
      ?T,?T,?T,?T,?T,?T
     >>.

hide_pixels() ->
    Pix = epx:pixmap_create(16, 16, argb),
    epx:pixmap_fill(Pix, {255,255,255,255}),
    epx_gc:set_fill_color(red),
    epx_gc:set_fill_style(solid),
    epx:draw_ellipse(Pix, {0,0,15,15}),
    epx_gc:set_foreground_color(white),
    epx:draw_line(Pix, {3,3}, {11,11}),
    epx:draw_line(Pix, {11,3}, {3,11}),
    Pix.

%%
%% GET and SET can bitrate on the can_usb backend
%%
setopts(#if_can{if_mod=Mod,if_pid=Pid}, Opts) when is_pid(Pid), is_list(Opts) ->
    Mod:setopts(Pid, Opts);
setopts(_, _Opts) ->
    error.

getopts(#if_can{if_mod=Mod,if_pid=Pid}, Opts) when is_pid(Pid), is_list(Opts) ->
    Mod:getopts(Pid, Opts);
getopts(_, Opts) ->
    [{Opt,undefined} || Opt <- Opts].
    
get_can_if() ->
    get_can_if_(can_router:interfaces()).

%% fixme export can_if from can_router!?
get_can_if_(IFs) ->
    get_can_if_(IFs, undefined).

get_can_if_([{can_if,IfPid,IfID,_Name,_Mon,
	      _Param=#{mod:=can_usb},
	      _Atime,_State} | _IFs], _Default) ->
    {ok,#if_can{if_mod=can_usb,if_id=IfID,if_pid=IfPid}};
get_can_if_([{can_if,IfPid,IfID,_Name,_Mon,
	      _Param=#{mod:=can_sock},
	      _Atime,_State} | _IFs], _Default) ->
    {ok,#if_can{if_mod=can_sock,if_id=IfID,if_pid=IfPid}};
get_can_if_([{can_if,IfPid,IfID,_Name,_Mon,
	      _Param=#{mod:=Mod},
	      _Atime,_State}|IFs], undefined) ->
    get_can_if_(IFs, #if_can{if_mod=Mod,if_id=IfID,if_pid=IfPid});
get_can_if_([_|IFs], Default) ->
    get_can_if_(IFs, Default);
get_can_if_([], undefined) ->
    {error, no_backend_found};
get_can_if_([], Default) ->
    {ok, Default}.
