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

-define(SCREEN_COLOR,            {0,255,255}). %% {192,192,192}).
-define(HIGH_COLOR,              {0,255,0}).
-define(LOW_COLOR,               {200,200,200}).   %% light gray
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
-define(FMT_HORIZONTAL_PAD, 4).
-define(FMT_VERTICAL_PAD,   3).

-record(fmt,
	{
	 dx :: integer(),        %% position relative #layout.x
	 dy :: integer(),        %% position relative #layout.y
	 dpos = 0 :: integer(),  %% row relative #layout.pos
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
	 background   = ?LAYOUT_BACKGROUND_COLOR,
	 foreground   = ?TEXT_COLOR,               %% text color
	 border       = ?FRAME_BORDER_COLOR,       %% frame border color
	 background1  = ?HIGHLIGHT_COLOR1,
	 foreground1  = ?TEXT_HIGHLIGHT_COLOR
	}).

-record(layout,
	{
	 key :: integer(),             %% frame key (id - (err+fd)
	 pos :: integer(),             %% list position/row
	 npos :: integer(),            %% layout occupy npos rows (wrapped)
	 pos0 :: integer(),            %% original list position
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

-record(frame,
	{
	 id,          %% integer 11 | 29 bit (ide=true) + EXT_BIT|RTR_BIT|FD
	 pid,         %% undefined or 24 bit pid
	 len=0,       %% length of data 0..8 (..60 for PID24 mode)
	 data=(<<>>)  %% binary with data bytes
	}).

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
	 pidmode = off,  %% off|pid24

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
	 frame,          %% ets: {frame_key(ID,PID),#frame{}}
	 frame_diff,     %% ets: {frame_key(ID,PID),[BitOffset]}
	 frame_layout,   %% ets: {frame_key(ID,PID),#layout{}}
	 frame_counter,  %% ets: {frame_key(ID,PID),Counter}
	 frame_freq,     %% ets: {frame_key(ID,PID),Time,OldCounter,String}
	 frame_anim,     %% ets: {{frame_key(ID,PID),FmtPos},Counter}
	 %% background_color :: epx:epx_color(),
	 menu_profile,
	 row_height    = 0,
	 next_pos = 1,
	 hide :: epx:epx_pixmap()
	}).

%% dynamic state elements
-record(d,
	{
	 tick :: undefined | reference(),
	 selection,                 %% current selection rect
	 selected = [],             %% selected frames [{FID,Pos}]
	 screen_color = ?SCREEN_COLOR,
	 content :: #window_content{},
	 auto_detect = undefined :: undefined | high | low,
	 auto_detect_tmr = undefined :: undefined | reference()
	}).

-define(TICK_INTERVAL, 100).

-define(AUTO_DETECT_INIT_INTERVAL, 3000).
-define(AUTO_DETECT_HIGH_INTERVAL, 2000).
-define(AUTO_DETECT_LOW_INTERVAL,  2000).
-define(AUTO_DETECT_EDGE_INTERVAL,  500).
-define(AUTO_DETECT_EDGE_GUESSING,  100).


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
		{screen_color,      ?SCREEN_COLOR},
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

    BitRates0 = proplists:get_value(bitrates, Env, ?DEFAULT_BITRATES),
    PidMode = proplists:get_value(pidmode, Env, off),

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
	   pidmode = PidMode,
	   if_can = IF,
	   font   = Font,
	   glyph_width  = W,
	   glyph_height = H,
	   glyph_ascent = epx:font_info(Font, ascent),
	   frame         = ets:new(frame, []),
	   frame_diff    = ets:new(frame_diff, []),
	   frame_layout  = ets:new(frame_layout, []),
	   frame_counter = ets:new(frame_counter, []),
	   frame_anim    = ets:new(frame_anim, []),
	   frame_freq    = ets:new(frame_freq, []),
	   menu_profile  = MProfile,
	   row_height    = RowHeight,
	   hide = hide_pixels()
	  },

    State = {S1, #d{ } },
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
%%   X              Hexadecimal format
%%   D              Decimal format
%%   B              Binary format
%%   O              Octal format
%%   C              Color format
%%   --
%%   G              Group selected bits
%%   Shift+G        Ungroup selected bits
%%   1-8            Split in groups of 1 to 8 bits
%%   Ctrl+S         Save information to /home/$USER/candy.txt
%%
%% Frames
%%   P              Toggle Pid mode off / pid24
%%   H              Sort Low -> High frames by frame id
%%   Ctrl+H         Sort High -> Low frames by frame id
%%
%%   up             Arrow up, scroll up
%%   down           Arrow down, scroll down
%%   pageup         Page up, scroll one page up
%%   pagedown       Page down, scroll one page down
%%
%%   A              Start/Stop auto detect
%%   T              Filter frames not used
%%   U              Undelete deleted frames
%%   
%% Global commands
%%   Q              Quit application
%%
%%   Ctrl+L         Listen mode toggle
%%   Ctrl+F         FD allow toggle
%%   Alt+1-9        Set bitrate 
%%   Alt+up         Bitrate up
%%   Alt+down       Bitrate down
%%   Ctrl+1-9[A-E]  Set datarate (FD)
%%   Ctrl+up        Datarate up
%%   Ctrl+down      Datarate down
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
    Keys = lists:usort([Key || {Key,_} <- Selected]),
    lists:foldl(fun(Key,Si) -> split_half_fid(Key,Selected,Si) end,
		State, Keys);
command($g, Selected, _Mod, _State = {S,D}) ->
    Keys = lists:usort([Key || {Key,_} <- Selected]),
    %% select min pos foreach FID and keep as selected
    Selected1 = lists:foldl(
		  fun(Key, Acc) ->
			  MinPos = lists:min([Pos || {K,Pos} <- Selected, 
						     K =:= Key]),
			  [{Key,MinPos}|Acc]
		  end, [], Keys),
    State1 = {S, D#d { selected = Selected1 }},
    lists:foldl(fun(Key,Si) -> merge_fid(Key, Selected, Si) end, State1, Keys);
command($h, _Selected, Mod, State) ->
    %% get list of {SortKey,NumberOfRows,LayoutKey}
    SNK =  
	fold_layout(
	  fun(Li, Acc, _Si) ->
		  case Li#layout.style of
		      deleted -> Acc;
		      hidden -> Acc;
		      collapsed -> Acc;
		      _ ->
			  if Mod#keymod.alt ->
				  %% sort according to original pos
				  [{Li#layout.pos0,Li#layout.npos,
				    Li#layout.key}|Acc];
			     true ->
				  %% sort according to key
				  [{0,Li#layout.npos,Li#layout.key}|Acc]
			  end
		  end
	  end, [], State),
    %% sort SNK according to sort key (reversed if ctrl is pressed)
    NK = 
	if Mod#keymod.ctrl ->
		[{Nj,Kj} || {_SortKey,Nj,Kj} <- lists:reverse(lists:sort(SNK))];
	   true ->
		[{Nj,Kj} || {_SortKey,Nj,Kj} <- lists:sort(SNK)]
	end,
    %% Generate a map of new layout positions
    KP = layout_pos_map(NK),
    State1 =
	each_layout(
	  fun(Li, Si) ->
		  case maps:get(Li#layout.key, KP, hidden) of
		      hidden ->
			  Si;
		      Pos ->
			  L2 = Li#layout { pos = Pos },
			  L3 = position_normal(L2, Si),
			  insert_layout(L3, Si)
		  end
	  end, State),
    epxw:invalidate(),
    State1;
%% Toggle pid mode
command($p, _Selected, _Mod, {S,D}) ->
    %% toggle pidmode
    NewMode = case S#s.pidmode of
		  off ->  pid24;
		  pid24 -> off
	      end,
    io:format("Toggle pid mode, new mode = ~p\n", [NewMode]),
    State1 = {S#s { pidmode = NewMode, next_pos = 1 }, D},
    set_status(State1),
    epxw:invalidate(),
    clear_frames(State1);
command($s, Selected, Mod, State={S,_D}) when Mod#keymod.ctrl ->
    Bytes = 
	case lists:usort([Key || {Key,_} <- Selected]) of
	    [Key|_] ->
		Sel = lists:filter(fun({F,_}) -> F =:= Key end, Selected),
		PosList = lists:sort([Pos || {_,Pos} <- Sel]),
		L = lookup_layout(Key, State),
		Format = L#layout.format,
		FmtList = select_fmts(1, tuple_to_list(Format), [], PosList),
		{ID,PID} = Key,
		Bs = [Fmt#fmt.bits || Fmt <- FmtList],
		io:format("SAVE KEY=0x~s PID=~p, Bits=~w\n",
			  [integer_to_list(ID,16), PID, Bs]),
		[[{BitPos,_Len}|_]|_] = Bs,
		format_candy(ID, PID, BitPos,
			     S#s.fd, S#s.bitrate, S#s.datarate);
	    _ -> []
	end,
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
    Keys = lists:usort([Key || {Key,_} <- Selected]),
    lists:foldl(fun(Key,Si) -> split_fid(Key, Selected, I-$0, Si) end, 
		State, Keys);
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
command($a, _Selected, _Mod, _State = {S,D}) ->
    case D#d.auto_detect_tmr of
	undefined -> %% start auto detect mode
	    epxw:profile_set(screen_color, ?LOW_COLOR),
	    epxw:invalidate(),
	    ets:delete_all_objects(S#s.frame_diff),
	    Tmr = erlang:start_timer(?AUTO_DETECT_INIT_INTERVAL,self(),high),
	    %% set auto_detect to undefined to dissallow detection until high
	    {S, D#d { auto_detect = undefined, auto_detect_tmr = Tmr,
		      screen_color = ?SCREEN_COLOR
		    } };
	TRef ->
	    epxw:profile_set(screen_color, ?SCREEN_COLOR),
	    epxw:invalidate(),
	    erlang:cancel_timer(TRef),
	    receive
		{timeout,TRef,_} -> ok
	    after 0 -> ok
	    end,
	    {S, D#d { auto_detect = undefined, 
		      auto_detect_tmr = undefined,
		      screen_color = ?SCREEN_COLOR }}
    end;
command($u, _Selected, _Mod, State) ->
    %% undelete all deleted frames
    Ls =
	fold_layout(
	  fun(L,Acc,_Si) ->
		  case L#layout.style of
		      deleted ->
			  [{undelete,L}|Acc];
		      _ -> Acc
		  end
	  end, [], State),
    {S1,D1} =
	lists:foldl(
	  fun({undelete,L}, Stt0={S,D}) ->
		  LPos = S#s.next_pos,
		  L1 = reposition_normal(LPos, L, Stt0),
		  NextPos = LPos+L1#layout.npos,
		  Stt1 = {S#s { next_pos = NextPos }, D},
		  insert_layout(L1, Stt1)
	  end, State, Ls),
    epxw:invalidate(),
    {S1,D1#d{ selected = [] }};
    
command($t, _Selected, _Mod, State={S,_}) ->
    Ls =
	fold_layout(
	  fun(L,Acc,_Si) ->
		  case L#layout.style of
		      deleted -> Acc;
		      hidden -> Acc;
		      collapsed -> Acc;
		      _ ->
			  Key = L#layout.key,
			  case ets:lookup(S#s.frame_diff, Key) of
			      [] -> [{delete,Key}|Acc]; 
			      [{_,[]}] -> [{delete,Key}|Acc];
			      [{_,Bs}] -> [{select,Key,Bs}|Acc]
			  end
		  end
	  end, [], State),
    {Selected,{S1,D1}} =
	lists:foldl(
	  fun({delete,Key}, {Sel,St0}) ->
		  L = lookup_layout(Key, St0),
		  {L1, St1} = remove_layout(L, St0),
		  St2 = insert_layout(L1, St1),
		  {Sel,St2};
	     ({select,K,_Bs}, {Sel,St0}) ->
		  %% FIXME: find fields matching bits in Bs?
		  {[{K,[?ID_FMT_POSITION]}|Sel], St0}
	  end, {[],State}, Ls),
    epxw:invalidate(),
    {S1,D1#d{ selected = Selected }};
command(Symbol, _Selected, Mod, State) ->
    io:format("unhandled command ~p\n", [Symbol]),
    {reply,{Symbol,Mod},State}.

format_candy(ID, undefined, BitPos, Fd, Bitrate, Datarate) ->
    ["0x",integer_to_list(ID,16),
     " ", integer_to_list(BitPos div 8),
     %% byte mask
     " 0x", integer_to_list((1 bsl (7-(BitPos rem 8))), 16),
     %% byte match value HIGH
     " 0x", integer_to_list((1 bsl (7-(BitPos rem 8))), 16),
     %% byte match value LOW
     " 0x00", "\n",
     "> canmode=0\n",
     "> canfd=", if Fd -> "1"; true -> "0" end, "\n",
     "> bitrate=", integer_to_list(Bitrate), "\n",
     if Fd ->
	     ["> datarate=", integer_to_list(Datarate), "\n"];
	true ->
	     ""
     end,
     "> pidmode=0\n"
    ];
format_candy(ID, PID, BitPos, _Fd, Bitrate, Datarate) ->
    ["0x",integer_to_list(ID,16),
     " ", integer_to_list(BitPos div 8),
     %% byte mask
     " 0x", integer_to_list((1 bsl (7-(BitPos rem 8))), 16),
     %% byte match value HIGH
     " 0x", integer_to_list((1 bsl (7-(BitPos rem 8))), 16),
     %% byte match value LOW
     " 0x00", "\n",
     "> canmode=0\n",
     "> canfd=1\n",
     "> bitrate=", integer_to_list(Bitrate), "\n",
     "> datarate=", integer_to_list(Datarate), "\n",
     "> pidmode=1\n",
     "> pid=0x", tl(integer_to_list(16#1000000+PID,16)), "\n"
    ].


%% form [ {NumberOfRows, LayoutKey} ] create new positions form 1...
layout_pos_map(NK) ->
    layout_pos_map_(NK, 1, []).

layout_pos_map_([{N,Key}|NK], Pos0, Acc) ->
    layout_pos_map_(NK, Pos0+N, [{Key,Pos0}|Acc]);
layout_pos_map_([], _Pos0, Acc) ->
    maps:from_list(Acc).

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
    Key = frame_key(FID,undefined),
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
	    key   = Key,
	    pos   = Pos,
	    style = fixed, %% do not collapse
	    format = Format
	   },
    NPos1 = Pos+1,
    State1 = {S#s{next_pos=NPos1}, D},
    L1 = position_layout(L0, State1),
    NPos2 = NPos1+L1#layout.npos,
    State2 = insert_layout(L1, {S#s{next_pos=NPos2},D}),
    ets:insert(S#s.frame_counter, {Key, 0}),
    Frame =  #frame{id=FID,len=8, data=(<<0,0,0,0,0,0,0,0>>) },
    ets:insert(S#s.frame, {Key,Frame}),
    redraw_by_key(Key, State2).

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
    State1 = handle_can_frames(insert_frame(Frame,[],State), State),
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

%% toggle auto_detect to high
handle_info({timeout,Ref,high}, {S,D}) when Ref =:= D#d.auto_detect_tmr ->
    epxw:profile_set(screen_color, ?HIGH_COLOR),
    TRef = erlang:start_timer(?AUTO_DETECT_HIGH_INTERVAL, self(), low),
    epxw:invalidate(),
    State1 = {S, D#d { auto_detect = high, auto_detect_tmr = TRef, 
		       screen_color = ?HIGH_COLOR }},
    {noreply, State1};
%% toggle auto_detect to low
handle_info({timeout,Ref,low}, {S, D}) when Ref =:= D#d.auto_detect_tmr ->
    epxw:profile_set(screen_color, ?LOW_COLOR),
    TRef = erlang:start_timer(?AUTO_DETECT_LOW_INTERVAL, self(), high),
    epxw:invalidate(),
    State1 = {S, D#d { auto_detect = low, auto_detect_tmr = TRef,
		       screen_color = ?LOW_COLOR }},
    {noreply, State1};

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
%% FIXME: check if the number of frames we collect is growing
%%  then start randomly through away some frames until stable
handle_can_frames(Frames, S) ->
    receive
	Frame = #can_frame {} ->
	    handle_can_frames(insert_frame(Frame,Frames,S), S)
    after 0 -> %% or a short time?
	    process_frames(lists:reverse(Frames),S,
			       false, #{})
    end.

insert_frame(#can_frame{id=ID,len=L,data=D}, Fs, _S) when
      ID band ?CAN_ERR_FLAG =:= ?CAN_ERR_FLAG ->
    [#frame{id=ID,len=L,data=D}|Fs];
insert_frame(#can_frame{id=ID,len=L,data=D}, Fs, {S,_D}) ->
    if S#s.pidmode =:= off; S#s.fd =:= false ->
	    [#frame{id=ID,len=L,data=D}|Fs];
       S#s.pidmode =:= pid24 ->
	    case insert_pid24_frames(#frame{id=ID},D,Fs) of
		error ->
		    if L =:= 8 ->
			    [#frame{id=ID,len=L,data=D}|Fs];
		       true ->
			    Fs
		    end;
		Fs1 -> Fs1
	    end
    end.

insert_pid24_frames(_F,<<0:24,_/binary>>,Fs) ->
    Fs;
insert_pid24_frames(F,<<Pid:24,L:8,Data/binary>>,Fs) ->
    if L > byte_size(Data) ->
	    error;  %% not a pid frame
       true ->
	    <<D:L/binary,Data1/binary>> = Data,
	    insert_pid24_frames(F,Data1,[F#frame{pid=Pid,len=L,data=D}|Fs])
    end;
insert_pid24_frames(_F,<<>>,Fs) -> Fs;
insert_pid24_frames(_F,<<0>>,Fs) -> Fs;
insert_pid24_frames(_F,<<0,0>>,Fs) -> Fs;
insert_pid24_frames(_F,<<0,0,0>>,Fs) -> Fs;
insert_pid24_frames(_F,_Bin,_Fs) -> error.

%% keep EFF/RTR flags, strip FD/ERR
-define(CAN_KEY_MASK, (?CAN_EFF_MASK bor ?CAN_EFF_FLAG bor ?CAN_RTR_FLAG)).

frame_key(FID,PID) ->
    {FID band ?CAN_KEY_MASK,PID}.

process_frames([#frame{id=FID}|Fs], {S,D},
	       _Redraw, RedrawSet) when
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
    process_frames(Fs, {S#s { if_error = Err }, D},
		       true, RedrawSet);
process_frames([Frame=#frame{id=FID,pid=PID}|Fs], State={S,D},
		   Redraw, RedrawSet) ->
    %% filter out extra flags err/fd ...
    Key = frame_key(FID,PID),
    case ets:lookup(S#s.frame, Key) of
	[{_,Frame}] -> %% no change
	    ets:update_counter(S#s.frame_counter, Key, 1),
	    process_frames(Fs, State, Redraw, RedrawSet);
	[{_,OldFrame}] ->
	    ets:insert(S#s.frame, {Key,Frame}),
	    ets:update_counter(S#s.frame_counter, Key, 1),
	    case diff_frames(FID,PID,Frame,OldFrame,State) of
		[] ->
		    process_frames(Fs, tick_restart(State),
				       Redraw, RedrawSet);
		Diff -> %% [LayoutPosList]
		    State1 = auto_detect_bits(Key, Frame, OldFrame, State),
		    KLi = [{Key,Li} || Li <- Diff],
		    [ ets:insert(S#s.frame_anim,{Ki,255})|| Ki <- KLi],
		    process_frames(Fs, tick_restart(State1),
				   Redraw,
				   RedrawSet#{ Key => true })
	    end;
	[] ->
	    ets:insert(S#s.frame, {Key,Frame}),
	    IDFmt =
		if FID band ?CAN_EFF_FLAG =/= 0 ->
			#fmt {field=id,base=16,type=unsigned,bits=[{4,29}]};
		   true ->
			#fmt {field=id,base=16,type=unsigned,bits=[{22,11}]}
		end,
	    PidFmt =
		if PID =:= undefined ->
			undefined;
		   true ->
			#fmt {field=pid,base=16,type=unsigned,bits=[{0,24}]}
		end,
	    Format =
		if FID band ?CAN_FD_FLAG =/= 0 ->
			default_fd_format(IDFmt,PidFmt);
		   true ->
			default_format(IDFmt)
		end,
	    [ ets:insert(S#s.frame_anim,{{Key,Pos},255}) ||
		Pos <- lists:seq(1,tuple_size(Format))  ],
	    LPos = S#s.next_pos,
	    L1 = #layout { key=Key, pos=LPos, pos0=LPos, format=Format },
	    L2 = position_layout(L1, State),

	    NextPos = LPos+L2#layout.npos,
	    State1 = {S#s{ next_pos = NextPos}, D},
	    State2 = insert_layout(L2, State1),
	    
	    ets:insert(S#s.frame_counter, {Key, 1}),
	    ets:insert(S#s.frame_freq, {Key,erlang:system_time(millisecond),1,""}),
	    process_frames(Fs, tick_restart(State2),
			   true, RedrawSet)
    end;
process_frames([], State, Redraw, RedrawSet) ->
    if Redraw ->
	    epxw:invalidate(),
	    State;
       true ->
	    case sets:size(RedrawSet) of
		0 ->
		    State;
		_Count ->
		    maps:fold(
		      fun(Key,_,Statei) ->
			      redraw_by_key(Key, Statei)
		      end, State, RedrawSet)
	    end
    end.

is_auto_detect({_,#d { auto_detect = Auto }}) -> Auto =/= undefined.

auto_detect_bits(Key, Frame, OldFrame, State={S,_D}) ->
    case is_auto_detect(State) of
	false ->
	    State;  %% bit detection is not running
	true ->
	    case is_auto_edge(State) of
		true -> %% Frame was received inside detection "window"
		    case ets:lookup(S#s.frame_diff,Key) of
			[] -> %% no bits yet, regard all switched bits
			    B1 = diff_bits(Frame,OldFrame),
			    ets:insert(S#s.frame_diff, {Key,B1}),
			    State;
			[{_,[]}] -> %% disabled
			    State;
			[{_,B0}] ->
			    B1 = diff_bits(Frame,OldFrame),
			    %% remove switched from 
			    B2 = B0--(B0--B1),
			    ets:insert(S#s.frame_diff, {Key,B2}),
			    %% io:format("~.16.0B: bits=~w\n", [Key,B2]),
			    State
		    end;
		false -> %% NewFrame was received outside detection "window"
		    case ets:lookup(S#s.frame_diff,Key) of
			[] -> %% no bits detected sofar
			    State;
			[{_,[]}] -> %% all bits are discarded already
			    State;
			[{_,B0}] -> %% current bits
			    B1 = diff_bits(Frame,OldFrame),
			    %% remove switch bits in B1 (switched)
			    B2 = B0 -- B1,
			    ets:insert(S#s.frame_diff, {Key,B2}),
			    %% io:format("~.16.0B: bits=~w\n", [Key,B2]),
			    State
		    end
	    end
		
    end.
    

%% return true if auto_detect was switched within edge interval!
is_auto_edge(_State={_,#d{ auto_detect = Auto, auto_detect_tmr = Tmr}}) 
  when is_reference(Tmr), Auto =/= undefined ->
    case erlang:read_timer(Tmr) of
	false -> false;
	Remain when Auto =:= high -> 
	    T = ?AUTO_DETECT_HIGH_INTERVAL-Remain,
	    (((T < 0) andalso (-T < ?AUTO_DETECT_EDGE_GUESSING)) 
	     orelse
	       ((T >= 0) andalso (T =< ?AUTO_DETECT_EDGE_INTERVAL)));
	Remain when Auto =:= low -> 
	    T = ?AUTO_DETECT_LOW_INTERVAL-Remain,
	    (((T < 0) andalso (-T < ?AUTO_DETECT_EDGE_GUESSING)) 
	     orelse
	       ((T >= 0) andalso (T =< ?AUTO_DETECT_EDGE_INTERVAL)))
    end;
is_auto_edge(_) ->
    false.

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
		    Key = Layout#layout.key,
		    Selected = 
			case lists:member({Key,I}, D#d.selected) of
			    true -> 
				lists:delete({Key,I},D#d.selected);
			    false ->
				[{Key,I} | D#d.selected]
			end,
		    D1 = D#d { selected = Selected },
		    State1 =  {S,D1},
		    redraw_by_key(Key, State1);
	       true ->
		    Key = Layout#layout.key,
		    deselect(State, [{Key,I}])
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
    Keys = lists:usort([Key || {Key,_} <- D#d.selected ++ NewSelected]),
    State = {S, D#d { selected = NewSelected }},
    lists:foldl(fun(Key,Si) -> redraw_by_key(Key, Si) end, State, Keys).


%% Set base to 'Base' in selected cells
set_base(Selected, Base, State) ->
    lists:foldl(
      fun({Key,I}, Si) ->
	      L= #layout{format=Format} = lookup_layout(Key,Si),
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
split_fid(Key, Selected, Size,State) ->
    Sel = lists:filter(fun({K,_}) -> K =:= Key end, Selected),
    PosList = lists:sort([Pos || {_,Pos} <- Sel]),
    L = lookup_layout(Key, State),
    Format = L#layout.format,
    FmtList1 = split_size(1, Size, tuple_to_list(Format), [], PosList),
    Format1 = list_to_tuple(FmtList1),
    L1 = L#layout { format=Format1 },
    set_anim(Key, L1, 0, State),
    update_layout(L, L1, State).

split_half_fid(Key, Selected, State={_S,_D}) ->
    Sel = lists:filter(fun({K,_}) -> K =:= Key end, Selected),
    PosList = lists:sort([Pos || {_,Pos} <- Sel]),
    L = lookup_layout(Key, State),
    Format = L#layout.format,
    FmtList1 = split_halfs(1, tuple_to_list(Format), [], PosList),
    Format1 = list_to_tuple(FmtList1),
    L1 = L#layout { format=Format1 },
    set_anim(Key, L1, 0, State),
    update_layout(L, L1, State).

merge_fid(Key, Selected, State) ->
    %% Sel is fields only from Key fields
    Sel = lists:filter(fun({K,_}) -> K =:= Key end, Selected),
    PosList = lists:sort([Pos || {_,Pos} <- Sel]),
    L = lookup_layout(Key, State),
    Format = L#layout.format,
    FmtList1 = merge_fmts(1, tuple_to_list(Format), [], PosList),
    Format1 = list_to_tuple(FmtList1),
    L1 = L#layout { format=Format1 },
    %% State1 = insert_layout(L1, State),  %% FIXME: check this
    set_anim(Key, L1, 0, State),
    update_layout(L, L1, State).

%% set animation value for data cells - fixme check fmt + data?
set_anim(Key, Layout, Value, _State={S,_D}) ->
    [ ets:insert(S#s.frame_anim,{{Key,Pos},Value}) ||
	Pos <- lists:seq(9, tuple_size(Layout#layout.format)) ].

%% merge selected data bit fields
merge_fmts(I,[Fmt1,Fmt2|FmtList],Acc,Sel) when 
      Fmt1#fmt.dpos =:= Fmt2#fmt.dpos,
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
    {S, D#d { tick = erlang:start_timer(?TICK_INTERVAL, self(), tick)}}.

tick_restart(State={_S,D}) when D#d.tick =:= undefined ->
    tick_start(State);
tick_restart(State) ->
    State.

-ifdef(not_used).
tick_stop(State={_S,D}) when D#d.tick =:= undefined ->
    State;
tick_stop(_State={S,D}) ->
    erlang:cancel_time(D#d.tick),
    {S, D#d { tick = undefined }}.
-endif.
    
default_format(IDFmt) ->
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
     #fmt { field=data, base=16, type=unsigned, bits=[{56,8}]}
    }.

default_fd_format(IDFmt,undefined) ->
    {
     #fmt { field=hide, type=undefined },
     IDFmt,
     #fmt { field=frequency,base=10,type={float,5,1},bits=[]},
     #fmt { field=id,   base=0,type={enum,{"-","F"}},bits=[{0,1}]},
     #fmt { field=id,   base=0,type={enum,{"-","X"}},bits=[{1,1}]},
     #fmt { field=id,   base=0,type={enum,{"-","R"}},bits=[{2,1}]},
     #fmt { field=id,   base=0,type={enum,{"-","E"}},bits=[{3,1}]},
     #fmt { field=len,  base=16,type=unsigned,bits=[{0,7}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{0,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{8,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{16,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{24,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{32,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{40,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{48,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{56,8}]},
     %% FD data
     #fmt { dpos=1,field=data, base=16, type=unsigned, bits=[{64,32}], ci=2},
     #fmt { dpos=1,field=data, base=16, type=unsigned, bits=[{96,32}], ci=3},

     #fmt { dpos=2,field=data, base=16, type=unsigned, bits=[{128,32}], ci=4},
     #fmt { dpos=2,field=data, base=16, type=unsigned, bits=[{160,32}], ci=5},

     #fmt { dpos=3,field=data, base=16, type=unsigned, bits=[{192,32}], ci=6},
     #fmt { dpos=3,field=data, base=16, type=unsigned, bits=[{224,32}], ci=7},

     #fmt { dpos=4,field=data, base=16, type=unsigned, bits=[{256,32}], ci=8},
     #fmt { dpos=4,field=data, base=16, type=unsigned, bits=[{288,32}], ci=8},

     #fmt { dpos=5,field=data, base=16, type=unsigned, bits=[{320,32}], ci=9},
     #fmt { dpos=5,field=data, base=16, type=unsigned, bits=[{352,32}], ci=9},

     #fmt { dpos=6,field=data, base=16, type=unsigned, bits=[{384,32}], ci=10},
     #fmt { dpos=6,field=data, base=16, type=unsigned, bits=[{416,32}], ci=10},

     #fmt { dpos=7,field=data, base=16, type=unsigned, bits=[{448,32}], ci=11},
     #fmt { dpos=7,field=data, base=16, type=unsigned, bits=[{480,32}], ci=11}
    };
default_fd_format(IDFmt,PIDFmt) ->
    {
     #fmt { field=hide, type=undefined },
     IDFmt,
     PIDFmt,
     #fmt { field=frequency,base=10,type={float,5,1},bits=[]},
     #fmt { field=id,   base=0,type={enum,{"-","F"}},bits=[{0,1}]},
     #fmt { field=id,   base=0,type={enum,{"-","X"}},bits=[{1,1}]},
     #fmt { field=id,   base=0,type={enum,{"-","R"}},bits=[{2,1}]},
     #fmt { field=id,   base=0,type={enum,{"-","E"}},bits=[{3,1}]},
     #fmt { field=len,  base=16,type=unsigned,bits=[{0,7}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{0,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{8,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{16,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{24,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{32,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{40,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{48,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{56,8}]}
    }.

%%
diff_frames(FID, PID, New, Old, State) ->
    Key = frame_key(FID, PID),
    #layout{format=Format} = Layout = lookup_layout(Key, State),
    if Layout#layout.style =:= deleted ->
	    [];
       true ->
	    case diff_frames_(1,Format,New,Old,[],State) of
		[] -> [];
		_Diff when Layout#layout.style =:= collapsed -> 
		    %% flash only the ID field
		    [?ID_FMT_POSITION];
		Diff -> Diff
	    end
    end.

diff_frames_(I,Format,New,Old,Acc,State) when I =< tuple_size(Format) ->
    Fmt = element(I, Format),
    BitsDataNew = get_bits(Fmt,New),
    BitsDataOld = get_bits(Fmt,Old),
    if BitsDataNew =:= BitsDataOld ->
	    diff_frames_(I+1,Format,New,Old,Acc,State);
       true ->
	    diff_frames_(I+1,Format,New,Old,[I|Acc],State)
    end;
diff_frames_(_I,_Format,_New,_Old,Acc,_State) ->
    Acc.

diff_bits(#frame{len=L,data=OldBits}, #frame{len=L,data=OldBits}) ->
    [];
diff_bits(#frame{len=L,data=NewBits}, #frame{len=L,data=OldBits}) ->
    Bits = crypto:exor(NewBits, OldBits),
    flipped(Bits);
diff_bits(#frame{len=L1,data=NewBits}, #frame{len=L2,data=OldBits}) ->
    D = L1 - L2,
    Bits = if D > 0 -> 
		   crypto:exor(NewBits, <<OldBits/binary, 0:D/unit:8>>);
	      D < 0 ->
		   crypto:exor(<<NewBits/binary, 0:(-D)/unit:8>>, OldBits)
	   end,
    flipped(Bits).

%% flipped_bits. return a list of bit offsets to flipped bits
flipped(Flipped) ->
    flipped_bits64(0, Flipped).

flipped_bits64(I, <<0:64,Rest/binary>>) -> flipped_bits64(I+64,Rest);
flipped_bits64(I, Rest) -> flipped_bits32(I,Rest).

flipped_bits32(I, <<0:32,Rest/binary>>) -> flipped_bits32(I+32,Rest);
flipped_bits32(I, Rest) -> flipped_bits8(I,Rest).

flipped_bits8(I,<<0:8,Rest/binary>>) -> flipped_bits8(I+8,Rest);
flipped_bits8(I,<<B:8/bitstring,Rest/binary>>) -> flipped_bits1(I,B,Rest);
flipped_bits8(_I, <<>>) -> [].

flipped_bits1(I,<<1:1,B/bitstring>>,Rest) -> [I|flipped_bits1(I+1,B,Rest)];
flipped_bits1(I,<<0:1,B/bitstring>>,Rest) -> flipped_bits1(I+1,B,Rest);
flipped_bits1(I,<<>>,Rest) -> flipped_bits64(I,Rest).


redraw_anim(Pixels,Area,State={S,_}) ->
    case ets:first(S#s.frame_anim) of
	'$end_of_table' ->
	    false;
	First ->
	    {Remove,Update} = redraw_anim_(Pixels,Area,First,[],[],State),
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

redraw_anim_(Pixels,Area,Iter={_Key,0}, Remove, Update, State={S,_}) ->
    %% redraw the entire layout using position interpolation
    Next = ets:next(S#s.frame_anim, Iter),
    redraw_anim_(Pixels,Area,Next,Remove,Update,State);
redraw_anim_(Pixels,Area,Iter={Key,Pos}, Remove, Update, State={S,_}) ->
    [{_,Frame}] = ets:lookup(S#s.frame, Key),
    Next = ets:next(S#s.frame_anim, Iter),
    case redraw_pos(Pixels,Area,Key,Pos,Frame,State) of
	true -> %% remove
	    redraw_anim_(Pixels,Area,Next, [Iter|Remove], Update, State);
	false ->
	    redraw_anim_(Pixels,Area,Next, Remove, [Iter|Update], State)
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
				    true ->
					case S#s.pidmode of
					    off -> ["FD"];
					    pid24 -> ["PID"]
					end;
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

intersect_layout(Layout, Area) ->
    R = {_,_,W,H} = epx_rect:intersect(Area,layout_rect(Layout)),
    if W =:= 0; H =:= 0 ->
	    false;
       true ->
	    R
    end.

%% redraw all Layouts 
redraw_all(Pixels, Area, State) ->
    each_layout(fun(Layout, State1) ->
			case intersect_layout(Layout, Area) of
			    false ->
				State1;
			    _Intersection ->
				redraw_layout_(Pixels, Layout, State1)
			end
		end, State).

redraw_by_key(Key, State={_S,_D}) ->
    epxw:draw(
      fun(Pixels, Area) ->
	      Layout = lookup_layout(Key, State),
	      case intersect_layout(Layout, Area) of
		  false ->
		      State;
		  _Intersection ->
		      redraw_layout_(Pixels, Layout, State)
	      end
      end).

each_layout(Fun, State={S,_D}) ->
    Tab = S#s.frame_layout,
    each_layout_(Fun, Tab, ets:first(Tab), State).

each_layout_(_Fun, _Tab, '$end_of_table', State) ->
    State;
each_layout_(Fun, Tab, Key, State) ->
    Layout = lookup_layout(Key, State),
    State1 = Fun(Layout, State),
    each_layout_(Fun, Tab, ets:next(Tab, Key), State1).

fold_layout(Fun, Acc, State={S,_D}) ->
    Tab = S#s.frame_layout,
    fold_layout_(Fun, Acc, Tab, ets:first(Tab), State).

fold_layout_(_Fun, Acc, _Tab, '$end_of_table', _State) ->
    Acc;
fold_layout_(Fun, Acc, Tab, Key, State) ->
    Layout = lookup_layout(Key, State),
    Acc1 = Fun(Layout, Acc, State),
    fold_layout_(Fun, Acc1, Tab, ets:next(Tab, Key), State).
    
redraw_layout_(Pixels, Layout, State={S,D}) ->
    #layout{ key=Key, color=Color,format=FmtTuple} = Layout,
    %% Count = ets:lookup_element(S#s.frame_counter, key, 2),
    draw_layout_background(Pixels, Layout, D#d.screen_color),
    [{_,Frame}] = ets:lookup(S#s.frame, Key),
    case Layout#layout.style of
	deleted ->
	    State;
	collapsed ->
	    _Remove = redraw_cell(Pixels,
				  Layout#layout.x,Layout#layout.y,
				  Key,?ID_FMT_POSITION,Color,
				  element(?ID_FMT_POSITION,FmtTuple),
				  Frame,State),
	    State;
	_ ->
	    redraw_cells(Pixels,
			 Layout#layout.x,Layout#layout.y,
			 Key,1,Color,FmtTuple,Frame,State)
    end.

redraw_cells(Pixels,Lx,Ly,Key,I,Color,FmtTuple,Frame,State) when 
      I =< tuple_size(FmtTuple) ->
    Fmt = element(I, FmtTuple),
    _Remove = redraw_cell(Pixels,Lx,Ly,Key,I,Color,Fmt,Frame,State),
    redraw_cells(Pixels,Lx,Ly,Key,I+1,Color,FmtTuple,Frame,State);
redraw_cells(_Pixels,_Lx,_Ly,_Key,_I,_Color,_FmtTuple,_Frame,State) ->
    State.


redraw_cell(Pixels,Lx,Ly,Key,I,_LayoutColor,Fmt,Frame,State={S,D}) ->
    #fmt {dx=Dx,dy=Dy,width=W,height=H,ci=Ci} = Fmt,
    Color = cell_color(Ci),
    X = Lx+Dx, Y = Ly+Dy,
    Anim = get_anim(Key,I,State),
    {Remove,TextColor} = highlight(Pixels,Anim,Color,{X,Y,W,H},State),
    BitsData = get_bits(Fmt, Frame),
    %% draw shape
    if Fmt#fmt.field =/= hide ->
	    epx_gc:set_fill_style(none),
	    epx_gc:set_foreground_color(Color#color.border),
	    epx_gc:set_border_color(Color#color.border),
	    epx_gc:set_border_style(inside),
	    case lists:member({Key,I}, D#d.selected) of
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
		ets:lookup(S#s.frame_freq,Key),
	    Td = Time1 - Time0,
	    String =
		if Td > 1000 ->
			[{_,Count1}] = 
			    ets:lookup(S#s.frame_counter,Key),
			N = Count1 - Count0,
			Ffmt = case Fmt#fmt.type of
				   {float,Fw,Fp} -> [$~,(Fw+$0),$.,(Fp+$0),$f];
				   {float,Fp} -> [$~,$.,(Fp+$0),$f]
			       end,
			String1 = lists:flatten(io_lib:format(Ffmt,
							      [(1000*N)/Td])),
			ets:insert(S#s.frame_freq,{Key,Time1,Count1,String1}),
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


%% draw layout return true if 'deleted' or false otherwise
redraw_pos(Pixels,Area,Key,I,Frame,State) ->
    Layout = lookup_layout(Key, State),
    #layout{ x=Lx, y=Ly, color=Color, format=FmtTuple} = Layout,
    case intersect_layout(Layout, Area) of
	false ->
	    case Layout#layout.style of
		collapsed ->
		    step_pos(Key,?ID_FMT_POSITION,State);
		deleted ->
		    true;
		_ ->
		    step_pos(Key,I,State)
	    end;
	_Intersection ->
	    case Layout#layout.style of
		collapsed ->
		    redraw_pos_(Pixels,Lx,Ly,Key,?ID_FMT_POSITION,
				Color,FmtTuple,Frame,State);
		deleted ->
		    true;
		_ ->
		    redraw_pos_(Pixels,Lx,Ly,Key,I,Color,FmtTuple,Frame,State)
	    end
    end.

%% step non-visible cells
step_pos(Key,I,State) ->
    Anim = get_anim(Key,I,State),
    if Anim =< 0 -> true;
       true -> false
    end.

redraw_pos_(Pixels,Lx,Ly,Key,I,Color,FmtTuple,Frame,State) ->
    if I > tuple_size(FmtTuple) ->
	    true;
       true ->
	    Fmt = element(I,FmtTuple),
	    redraw_cell(Pixels,Lx,Ly,Key,I,Color,Fmt,Frame,State)
    end.



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
draw_layout_background(Pixels, Layout, Color) ->
    draw_layout_rectangle(Pixels, Layout, Color, 
			  Layout#layout.width).

%% prepare layout by painting the layout background color
clear_layout_background(Pixels, Layout, _State={_S,D}) ->
    Color = D#d.screen_color,
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

get_anim(Key,I,{S,_}) ->
    case ets:lookup(S#s.frame_anim, {Key,I}) of
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

-ifdef(not_used).
interp_coord(Val, {X1,Y1}, {X0,Y0}) ->
    F1 = Val/255,
    F0 = 1-F1,
    X = trunc(F1*X1 + F0*X0),
    Y = trunc(F1*Y1 + F0*Y0),
    {X,Y}.
-endif.

get_bits(#fmt { type={string,NameBits} }, _Frame) ->
    NameBits;
get_bits(#fmt { field=frequency }, _Frame) -> 
    <<>>;
get_bits(#fmt { field=hide }, _Frame) -> 
    <<>>;
get_bits(Fmt, Frame) ->
    Data = case Fmt#fmt.field of
	       data -> extend_bits(Frame#frame.data, 64);
	       id   -> <<(Frame#frame.id):33>>;  %% 33 bits!!! FD support
	       pid  -> <<(Frame#frame.pid):24>>;
	       len  -> <<(Frame#frame.len):7>>
	   end,
    collect_bits(Fmt#fmt.bits, Data).

extend_bits(Data, MinSize) when bit_size(Data) >= MinSize ->
    Data;
extend_bits(Data, MinSize) ->
    Size = bit_size(Data),
    Pad = MinSize - Size,
    <<Data/bitstring, 0:Pad>>.

layout_rect(#layout { x=X, y=Y, width=W, height=H }) ->
    {X,Y,W,H}.

invalidate_layout(Layout, State) ->
    #layout { x=X0, y=Y0, width=W0, height=H0 } = Layout,
    epxw:invalidate({X0,Y0,W0,H0}),
    State.

%% Layout is to be "delete", 
%% reposition layouts below the Layout, update next_pos
%% update view_height / view_width

remove_layout(Layout, State={S,_D}) ->
    Pos = Layout#layout.pos,  %% row to deleted
    N = Layout#layout.npos,   %% number of rows deleted
    io:format("remove: pos=~w, n=~w\n", [Pos,N]),
    State1 = move_layout_up(Pos, N, State),
    Key = Layout#layout.key,
    lists:foreach(fun(I) ->
			  ets:delete(S#s.frame_anim, {Key,I})
		  end, lists:seq(1, tuple_size(Layout#layout.format))),
    {S1,D1} = State1,
    S2 = S1#s{ next_pos = S1#s.next_pos-N },
    State2 = {S2, D1},
    {Layout#layout { style=deleted,pos0=0,pos=0,x=0,y=0},State2}.

%% Move all layouts below layout at Pos with N rows
%% upwards N steps
move_layout_up(Pos, N, State) ->
    %% first collect Frame layouts below Pos
    Ls = fold_layout(
	   fun(L, Acc, _Si) ->
		   if L#layout.style =:= deleted ->
			   Acc;
		      L#layout.pos > Pos ->
			   [{L#layout.pos,L#layout.key}|Acc];
		      true ->
			   Acc
		   end
	   end, [], State),
    %% Now move the frames N rows up
    lists:foldl(
      fun({_Pos,Key}, Si) ->
	      L = lookup_layout(Key, Si),
	      Pos1 = L#layout.pos,
	      NPos = Pos1 - N,
	      io:format("move ~w to ~w\n", [Pos1, NPos]),
	      L1 = L#layout { pos0 = NPos, pos = NPos },  
	      L2 = reposition_normal(Pos1, L1, Si),
	      insert_layout(L2, Si)
      end, State, lists:sort(Ls)).
	      

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

position_to_coord(Pos, {S,_D}) when is_integer(Pos), Pos > 0 ->
    {0, (Pos-1)*(S#s.row_height+?FMT_VERTICAL_PAD)}.

%% recalulate Layout (calculate rows and coordinates)
position_layout(Layout, State) ->
    case Layout#layout.style of
	normal    -> position_normal(Layout,State);
	fixed     -> position_normal(Layout,State);
	collapsed -> position_collapsed(Layout,State);
	deleted   -> position_deleted(Layout,State)
    end.

position_deleted(Layout, _State) ->
    Layout#layout { x=0,y=0,npos=0 }.

reposition_normal(Pos, L, State={_S,_D}) ->
    {X,Y} = position_to_coord(Pos, State),
    L#layout { style=normal, pos=Pos, pos0=Pos, x=X,y=Y }.

position_normal(Layout, State={_S,_D}) ->
    {X,Y} = position_to_coord(Layout#layout.pos, State),
    {Width,Height,FmtTuple,Dp} = position_cells(Layout#layout.format, State),
    Layout#layout { x=X,y=Y,npos=Dp,width=Width,height=Height,format=FmtTuple }.

position_collapsed(Layout, _State={S,_D}) ->
    J = Layout#layout.pos-1,
    Fmt = element(?ID_FMT_POSITION, Layout#layout.format), %% FIXME!
    Row = 1,  %% calculate row from bottom left to right
    Height = S#s.row_height,
    Num    = num_glyphs(Fmt),
    GW     = glyph_width(S),
    Wide   = 8*GW + 4,
    Width  = Num*GW + 4,
    X      = J*Wide,
    Y      = epxw:height() - Row*Height,
    Fmt1 = Fmt#fmt { dx=0, dy=0, width=Width, height=Height },
    FmtTuple = setelement(?ID_FMT_POSITION, Layout#layout.format, Fmt1),
    Layout#layout { x=X,y=Y,npos=1,width=Wide+2,height=Height,format=FmtTuple}.

position_cells(FmtTuple, State) ->
    Dx=0, Dy=0, Dpos=0,
    position_cells(1,Dx,Dy,Dpos,0,FmtTuple,[],State).

position_cells(I,Dx,Dy,Dpos,MaxW,FmtTuple,Acc,State={S,_D}) 
  when I =< tuple_size(FmtTuple) ->
    Fmt = element(I, FmtTuple),
    Width = fmt_width(Fmt, S), 
    Height = S#s.row_height,
    if Dpos =:= Fmt#fmt.dpos ->
	    Fmt1 = Fmt#fmt { dx=Dx, dy=Dy, width=Width, height=Height },
	    position_cells(I+1,Dx+Width+2,Dy,Dpos,MaxW,FmtTuple,
			   [Fmt1|Acc],State);
       true ->
	    MaxW1 = max(MaxW,Dx-2),
	    Dy1 = Dy + Height + ?FMT_VERTICAL_PAD,
	    %% FIXME: calc once! assumes fmt 2-8 are on same row!
	    Dx1 = fmt_width(element(1,FmtTuple),S) + 
		       fmt_width(element(2,FmtTuple),S) + 
		       fmt_width(element(3,FmtTuple),S) + 
		       fmt_width(element(4,FmtTuple),S) + 
		       fmt_width(element(5,FmtTuple),S) + 
		       fmt_width(element(6,FmtTuple),S) + 
		       fmt_width(element(7,FmtTuple),S) + 
		       fmt_width(element(8,FmtTuple),S) + 2*8,
	    Fmt1 = Fmt#fmt { dx=Dx1, dy=Dy1, width=Width, height=Height },
	    position_cells(I+1,Dx1+Width+2,Dy1,Fmt#fmt.dpos,MaxW1,
			   FmtTuple,[Fmt1|Acc],
			   State)
    end;
position_cells(I,Dx,Dy,Dpos,MaxW,_FmtTuple,Acc,_State={S,_D}) ->
    FmtTuple1 = list_to_tuple(lists:reverse(Acc)),
    if I =:= 1 -> {0,0,FmtTuple1,0};
       true -> {max(MaxW,Dx-2),Dy+S#s.row_height,FmtTuple1,Dpos+1}
    end.

fmt_width(Fmt,S) ->
    %% size of base indication text width (for data entries)
    Wind = if Fmt#fmt.field =:= data -> 6;
	      true -> 0
	   end,
    GW = glyph_width(S),
    Num = num_glyphs(Fmt),
    Wind + Num*GW +?FMT_HORIZONTAL_PAD.

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

%% remove all frames
clear_frames(State={S,_D}) ->
    ets:delete_all_objects(S#s.frame),
    ets:delete_all_objects(S#s.frame_layout),
    ets:delete_all_objects(S#s.frame_anim),
    ets:delete_all_objects(S#s.frame_freq),
    ets:delete_all_objects(S#s.frame_counter),
    State.

%% FID maybe frame id or internal key (where fd/err is stripped)
lookup_layout(Key,_State={S,_D}) ->
    [{_,L}] = ets:lookup(S#s.frame_layout,Key),
    L.

update_layout(OldLayout, NewLayout, State={_S,_D}) ->
    %% Pixels = epxw:pixels(),
    epxw:draw(
      fun(Pixels, _Area) ->
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
	      invalidate_layout(Layout, State2)
      end).


insert_layout(Layout, State={S,_D}) ->
    ets:insert(S#s.frame_layout, {Layout#layout.key,Layout}),
    %% FIXME: use smarter calculation!
    {_,_,W,H} = epxw:view_rect(),
    epxw:set_view_size(max(W,Layout#layout.x+Layout#layout.width),
		       max(H,Layout#layout.y+Layout#layout.height)),
    State.


layout_from_coord(XY, _State={S,_D}) ->
    Tab = S#s.frame_layout,
    Key = ets:first(Tab),
    layout_from_coord_(XY, Tab, Key).

layout_from_coord_(_XY, _Tab, '$end_of_table') ->
    false;
layout_from_coord_(XY, Tab, Key) ->
    case ets:lookup(Tab, Key) of
	[] ->
	    layout_from_coord_(XY, Tab, ets:next(Tab, Key));
	[{_,Layout=#layout{x=X1,y=Y1,width=W,height=H}}] ->
	    {X,Y} = XY,
	    if X >= X1, Y >= Y1, X < X1+W, Y < Y1+H ->
		    Layout;
	       true ->
		    layout_from_coord_(XY, Tab, ets:next(Tab, Key))
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
