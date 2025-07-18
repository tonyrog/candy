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

-compile(export_all).
%% API
-export([start0/0]).  %% make app image target
-export([start/0, start/1]).
-export([status/0]).
-export([install/0, install_cmds/0]).

%% debug
-export([select_rate/2]).
-export([rate_to_list/1]).
-export([format_layout/1]).
-export([format_record/2]).

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

-define(ld(K, E, D), proplists:get_value((K), (E), (D)#profile.K)).
-define(ldc(S, K, E, D), epx_profile:color((S), ?ld(K,(E),(D)))).

%% some text colors
-define(TEXT_BLACK,   {0,0,0,0}).
-define(TEXT_BLUE,    {0,0,0,255}).
-define(TEXT_GREEN,   {0,0,255,0}).
-define(TEXT_CYAN,    {0,0,255,255}).
-define(TEXT_RED,     {0,255,0,0}).
-define(TEXT_MAGENTA, {0,255,0,255}).
-define(TEXT_YELLOW,  {0,255,255,0}).
-define(TEXT_WHITE,   {0,255,255,255}).


-define(RATE_MENU_COLOR, blue).

-define(SCREEN_COLOR,            {0,255,255}).
-define(HIGH_COLOR,              {0,255,0}).
-define(LOW_COLOR,               {200,200,200}).   %% light gray
-define(LEFT_OFFSET,             8).               %% left offset to content
-define(RIGHT_OFFSET,            8).               %% right offset to content
-define(TOP_OFFSET,              8).               %% top offset to content
-define(BOTTOM_OFFSET,           8).               %% bottom offset to content
-define(LAYOUT_BACKGROUND_COLOR, {210,210,210}).   %%

-define(FMT_HORIZONTAL_PAD,        4).
-define(FMT_VERTICAL_PAD,          4).
-define(FMT_BORDER_WIDTH,          1).
-define(FMT_SELECTED_BORDER_WIDTH, 2).
-define(LAYOUT_BORDER_WIDTH,       4).
-define(LAYOUT_X_PAD,              8).
-define(LAYOUT_Y_PAD,              8).
-define(LAYOUT_BORDER_COLOR,     {150,150,150}).      %%
-define(TEXT_BACKGROUND_COLOR,   {255,255,255}).   %% white
-define(FRAME_BORDER_COLOR,      {0,0,0}).         %% black border
-define(TEXT_COLOR,              ?TEXT_BLACK).       %% black text
-define(TEXT_HIGHLIGHT_COLOR,    ?TEXT_WHITE). %% white hight light
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

-define(TOP_FONT_NAME, "Arial").
-define(TOP_FONT_SIZE, 12).


%% profile with default values
-record(profile,
	{
	 scheme                        = logo, %% xterm,
	 %%
	 screen_color                  = ?SCREEN_COLOR,
	 %% menu_info
	 menu_font_name                = "Arial",
	 menu_font_size                = 14,
	 menu_font_color               = grey5,   %% light
	 menu_background_color         = grey10,  %% dark
	 menu_border_color             = grey1,
	 menu_border_width             = 2,

	 %% fields
	 frame_background_color = ?TEXT_BACKGROUND_COLOR,
	 frame_foreground_color = ?TEXT_COLOR,
	 frame_border_color     = ?FRAME_BORDER_COLOR,
	 frame_background1_color = ?HIGHLIGHT_COLOR1,
	 frame_foreground1_color = ?TEXT_HIGHLIGHT_COLOR,
	 
	 %% layout
	 layout_background_color = ?LAYOUT_BACKGROUND_COLOR,
	 layout_border_color     = ?LAYOUT_BORDER_COLOR
	}).


-define(APP, ?MODULE).
-define(APPSTR, ?MODULE_STRING).
-define(DOTAPP, [$.|?APPSTR]).
-define(APPPNG, ?APPSTR++".png").
-define(APPDSK, ?APPSTR++".desktop").

-define(verbose(F,A), ok).
%% -define(verbose(F,A), io:format((F),(A))).

-record(fmt,
	{
	 x :: integer(),         %% position relative #layout.x
	 y :: integer(),         %% position relative #layout.y
	 row = 0 :: integer(),   %% row position in layout (0=first)
	 width :: non_neg_integer(),
	 height :: non_neg_integer(),
	 %% hidden = false :: boolean(),
	 type = unsigned :: unsigned | signed | undefined |
			    {enum,tuple()} | {string,string()},
	 field = none :: none | id | len | data | frequency | select,
	 base = 16 :: 0 | 2 | 8 | 16 | 10,
	 signed = false :: boolean(),
	 ci = 1 :: 1..13,               %% color index in cell_color(I)
	 bits = [] :: [{Pos::non_neg_integer(),Length::non_neg_integer()}]
	}).

%% can frame color
-record(color,
	{
	 layout_background = ?LAYOUT_BACKGROUND_COLOR,
	 layout_border     = ?LAYOUT_BORDER_COLOR,
	 background   = ?TEXT_BACKGROUND_COLOR,
	 foreground   = ?TEXT_COLOR,               %% text color
	 border       = ?FRAME_BORDER_COLOR,       %% frame border color
	 background1  = ?HIGHLIGHT_COLOR1,
	 foreground1  = ?TEXT_HIGHLIGHT_COLOR
	}).

%% FrameID = (can.id - (err+fd))
%% ProtcolID = Can PID
-type framekey() :: {FrameID::integer(),ProtocolID::integer()|undefined}.

-record(layout,
	{
	 key   :: framekey(),
	 %% pos   :: integer(),          %% list position/row
	 %% n     :: integer(),          %% layout occupy n rows
	 color = #color{} :: #color{},   %% color profile
	 x     :: integer(),             %% x offset
	 y     :: integer(),             %% y offset
	 width :: non_neg_integer(),     %% total width
	 height :: non_neg_integer(),    %% total height
	 format = {} :: tuple()        %% {#fmt{},...}
	}).
-type layout()::#layout{}.

-define(ID_FMT_POSITION, 2).

%% keep sorted
-define(DEFAULT_BITRATES, [125000, 250000, 500000, 1000000]).
-define(DEFAULT_DATARATES, [500000, 1000000, 5000000, 10000000]).

-define(DEFAULT_SAMPLE_POINT, undefined).  %% driver default value
-define(MIN_SAMPLE_POINT, 500).
-define(MAX_SAMPLE_POINT, 900).
-define(SAMPLE_POINT_STEP, 5).

-define(DEFAULT_DATA_SAMPLE_POINT, undefined).  %% driver default value
-define(MIN_DATA_SAMPLE_POINT, 500).
-define(MAX_DATA_SAMPLE_POINT, 900).

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
	 app_name = "Candy",
	 fd = undefined,                  %% fd mode
	 listen_only = undefined,         %% listen only mode
	 bitrate = undefined,
	 bitrates = ?DEFAULT_BITRATES,
	 datarate = undefined,
	 datarates =  ?DEFAULT_DATARATES,
	 sample_point = undefined,        %% in percent (integer 50..90)
	 data_sample_point = undefined,   %% in percent (integer 50..90)
	 conf_bitrates = ?DEFAULT_BITRATES,
	 conf_datarates = ?DEFAULT_DATARATES,
	 pidmode = off,  %% off|pid24
	 canfilter = false,  %% toggle with "F"
	 pause = false :: boolean(),  %% pause frame reception
	 if_state  = down,            %% up | down
	 if_param  = #{},             %% current interface params (when up)
	 if_error  = [],              %% interface error code list
	 if_can :: #if_can{},
	 nrows = 30 :: integer(),     %% number or rows shown
	 font   :: epx:epx_font(),
	 glyph_width,
	 glyph_height,
	 glyph_ascent,
	 %%
	 top_font   :: epx:epx_font(),
	 top_glyph_width,
	 top_glyph_height,
	 top_glyph_ascent,
	 %%
	 frame,          %% ets: {framekey(),#frame{}}
	 frame_diff,     %% ets: {framekey(),[BitOffset]}
	 frame_counter,  %% ets: {framekey(),Counter}
	 frame_freq,     %% ets: {framekey(),Time,OldCounter,String}
	 frame_anim,     %% ets: {{framekey(),FmtPos},Counter}
	 frame_move,     %% ets: {framekey(),Dy,Step}
	 %% background_color :: epx:epx_color(),
	 menu_profile,
	 color_profile = #color{} :: #color{},
	 help_menu,
	 row_height = 0
	}).

%% dynamic state elements
-record(d,
	{
	 tick :: undefined | reference(),
	 move :: undefined | reference(),
	 selection,                 %% current selection rect
	 selected = [],             %% selected frames [{FID,Pos}]
	 orig_screen_color = ?SCREEN_COLOR,  %% original sceen mode (auto)
	 screen_color = ?SCREEN_COLOR,       %% current screen color
	 content :: #window_content{},
	 auto_key = released :: pressed | released,
	 auto_detect = undefined :: undefined | high | low,
	 auto_detect_tmr = undefined :: undefined | reference(),
	 help = false :: boolean(),
	 active = #{} :: #{ framekey() => #layout{}},
	 hidden = #{} :: #{ framekey() => #layout{}},
	 active_order = [] :: [ Key::framekey() ],  %% current display order
	 frame_count  :: integer(),  %% previous frame count
	 frame_time   :: integer()   %% previous frame time
	}).
-type state() :: {#s{},#d{}}.

-define(TICK_INTERVAL, 100).
-define(MOVE_INTERVAL, 20).
-define(MOVE_TIME_MS,  500).

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


help_menu() ->
    [
     {"Pause",             "Space"},
     {"AutoKey",           "Tab"},
     {"Group bits",        "G"},
     {"Ungroup bits",      "Shift+G"},
     {"Ungroup 1-bit",     "1"},
     {"Ungroup 2-bit",     "2"},
     {"Ungroup 4-bit",     "4"},
     {"Ungroup 8-bit",     "8"},
     {"---"},     
     {"Binay",             "B"},
     {"Hexadecimal",       "X"},
     {"Decimal",           "D"},
     {"Color",             "C"},
     {"---"},
     %% 
     {"Toggle Pid mode",   "P"},
     {"Sort Low to High",  "H"},
     {"Sort High to Low",  "Ctrl+H"},
     {"---"},
     {"Auto Detect",       "A"},
     {"Select Detected",   "T"},
     {"Undelete",          "U"},
     {"Filter selected",   "F"},
     {"Clear",             "Ctrl+K"},
     {"Delete",            "Delete"},  %% or backspace?
     {"---"},
     {"Toggle Listen mode", "Ctrl+L"},
     {"Toggle FD mode",     "Ctrl+F"},
     {"---"},
     %% Save information to /home/$USER/candy.txt
     {"Save",               "Ctrl+S"},
     {"Quit",               "Ctrl+Q"}
    ].
%%
%%   up             Arrow up, scroll up
%%   down           Arrow down, scroll down
%%   pageup         Page up, scroll one page up
%%   pagedown       Page down, scroll one page down
%%
%% Global commands
%%   SPACE          Pause can frame reception / (drop frames)
%%   TAB            Auto key, detect frames the falls during key press
%%   R              Refresh screen
%%
%%   Alt+up         next bitrate
%%   Alt+down       prev bitrate
%%   Shift+Alt+up   next sample point + 5  [500..900]
%%   Shift+Alt+down prev sample point - 5  [500..900]
%%
%%   Ctrl+up        next datarate
%%   Ctrl+down      prev datarate
%%   Shift+Ctrl+up  next data sample point + 5  [500...900]
%%   Shift+Ctrl+down prev data sample point - 5 [500...900]
%%

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

start_it(Options0, Start) ->
    Options = Options0 ++ application:get_all_env(candy),
    Profile = proplists:get_value(profile, Options, []),
    epxw:Start(?MODULE,
	       Options,
	       Profile ++
	       [{title, "Candy"},
		{scroll_horizontal, bottom},  %% none|top|bottom
		{scroll_vertical,   right},   %% none|left|right
		{scroll_bar_color,  cyan},
		{scroll_hndl_color, blue},
		{scroll_bar_size,   14},
		{scroll_hndl_size,  10},
		{screen_color, ?SCREEN_COLOR},
		{left_bar, 0},
		{right_bar, 0},
		{top_bar, 28},
		{width,  ?WIDTH},
		{height, ?HEIGHT},
		{view_width,?WIDTH},
		{view_height,?HEIGHT-(20+18)}
	       ]).
    
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
    can_router:attach(),
    can_router:error_reception(on),
    can_router:if_state_supervision(on),
    S0 = #s{},
    Env = proplists:get_value(profile, Options, []),
    FontName = proplists:get_value(font_name, Env, ?FONT_NAME),
    FontSize = proplists:get_value(font_size, Env, ?FONT_SIZE),
    TopFontName = proplists:get_value(top_font_name, Env, ?TOP_FONT_NAME),
    TopFontSize = proplists:get_value(top_font_size, Env, ?TOP_FONT_SIZE),
    {ok,Font} = epx_font:match([{name,FontName},{size,FontSize}]),
    {ok,TopFont} = epx_font:match([{name,TopFontName},{size,TopFontSize},
				   {weight,bold}]),
    epx_gc:set_font(TopFont),
    {TW,TH}  = epx_font:dimension(TopFont,"0"),

    epx_gc:set_font(Font),
    {W,H}  = epx_font:dimension(Font,"0"),

    Conf_BitRates = proplists:get_value(bitrates, Options,
					?DEFAULT_BITRATES),
    Conf_DataRates = proplists:get_value(datarates, Options,
					 ?DEFAULT_DATARATES),
    PidMode = proplists:get_value(pidmode, Options, off),

    Profile = load_profile(Env),
    MProfile = load_menu_profile(Profile),
    CProfile = load_color_profile(Profile),
    AppName = proplists:get_value(title, Env, "Candy"),
    HelpMenu = epx_menu:create(MProfile, help_menu()),

    Window = epxw:window(),
    epx:window_enable_events(Window, no_auto_repeat),

    RowHeight = H + 2,

    {IF,
     [{listen_only,LISTEN},{fd,FD},
      {bitrate,BitRate0}, {bitrates, BitRates0},
      {datarate,DataRate0}, {datarates, DataRates0},
      {sample_point,SamplePoint0},{data_sample_point,DataSamplePoint0}
     ]} =
	case get_can_if() of
	    {ok,IFCan=#if_can{if_mod=Mod,if_pid=Pid}} when is_pid(Pid) ->
		{IFCan, Mod:getopts(Pid, [listen_only, fd,
					  bitrate, bitrates, 
					  datarate, datarates,
					  sample_point,data_sample_point
					 ])};
	    {error,_} ->
		{#if_can{if_mod=undefined,if_id=0,if_pid=undefined},
		 [{listen_only, false},
		  {fd, false},
		  {bitrate, hd(Conf_BitRates)},
		  {bitrates, Conf_BitRates},
		  {datarate, undefined},
		  {datarates, Conf_DataRates},
		  {sample_point, undefined},
		  {data_sample_point, undefined}
		 ]}
	end,
    {BitRate, BitRates} = 
	make_rates(BitRate0, BitRates0, Conf_BitRates),
    {DataRate, DataRates} = 
	make_rates(DataRate0, DataRates0, Conf_DataRates),

    SamplePoint = sample_point_to_percent(SamplePoint0),
    DataSamplePoint = sample_point_to_percent(DataSamplePoint0),

    S1 = S0#s {
	   app_name = AppName,
	   listen_only = LISTEN,
	   fd = FD,
	   bitrate = BitRate,
	   bitrates = BitRates,
	   datarate = DataRate,
	   datarates = DataRates,
	   conf_bitrates = Conf_BitRates,
	   conf_datarates = Conf_DataRates,
	   sample_point = trim_sample_point(SamplePoint),
	   data_sample_point = trim_data_sample_point(DataSamplePoint),
	   pidmode = PidMode,
	   if_can = IF,

	   font   = Font,
	   glyph_width  = W,
	   glyph_height = H,
	   glyph_ascent = epx:font_info(Font, ascent),

	   top_font = TopFont,
	   top_glyph_width = TW,
	   top_glyph_height = TH,
	   top_glyph_ascent = epx:font_info(TopFont, ascent),

	   frame         = ets:new(frame, []),
	   frame_diff    = ets:new(frame_diff, []),
	   %% frame_layout  = ets:new(frame_layout, []),
	   frame_counter = ets:new(frame_counter, []),
	   frame_anim    = ets:new(frame_anim, []),
	   frame_move    = ets:new(frame_move, []),
	   frame_freq    = ets:new(frame_freq, []),
	   menu_profile  = MProfile,
	   color_profile = CProfile,
	   help_menu     = HelpMenu,
	   row_height    = RowHeight
	  },

    ets:insert(S1#s.frame_counter, {all,0}),

    D1 = #d{ orig_screen_color = Profile#profile.screen_color,
	     screen_color = Profile#profile.screen_color,
	     frame_count  = 0,
	     frame_time   = erlang:system_time(millisecond)
	   },
    State = {S1, D1},
    set_status(State),
    {ok, State}.

%% load "local" epxw options from options+environment
load_profile(E) ->
    D = #profile{},  %% defaults
    S = ?ld(scheme, E, D),

    #profile {
       scheme = S,
       screen_color = ?ldc(S,screen_color, E, D),
       menu_font_name = ?ld(menu_font_name, E, D),
       menu_font_size = ?ld(menu_font_size, E, D),
       menu_font_color = ?ldc(S,menu_font_color, E, D),
       menu_background_color = ?ldc(S,menu_background_color, E, D),
       menu_border_color = ?ldc(S,menu_border_color, E, D),
       menu_border_width = ?ld(menu_border_width, E, D),
       %% frames
       frame_background_color = ?ldc(S, frame_background_color, E, D),
       frame_foreground_color = ?ldc(S, frame_foreground_color, E, D),
       frame_border_color     = ?ldc(S, frame_border_color, E, D),
       frame_background1_color = ?ldc(S, frame_background1_color, E, D),
       frame_foreground1_color = ?ldc(S, frame_foreground1_color, E, D),
       %% layouts
       layout_background_color = ?ldc(S, layout_background_color, E, D),
       layout_border_color = ?ldc(S, layout_border_color, E, D)
      }.

load_menu_profile(Profile) ->
    #menu_profile {
       scheme           = Profile#profile.scheme,
       font_name        = Profile#profile.menu_font_name,
       font_size        = Profile#profile.menu_font_size,
       font_color       = Profile#profile.menu_font_color,
       background_color = Profile#profile.menu_background_color,
       border_color     = Profile#profile.menu_border_color,
       border_width     = Profile#profile.menu_border_width
      }.

trim_sample_point(Pt) when is_float(Pt) ->
    max(min(Pt, ?MAX_SAMPLE_POINT), ?MIN_SAMPLE_POINT);
trim_sample_point(_) ->
    ?DEFAULT_SAMPLE_POINT.

trim_data_sample_point(Pt) when is_float(Pt) ->
    max(min(Pt, ?MAX_DATA_SAMPLE_POINT), ?MIN_DATA_SAMPLE_POINT);
trim_data_sample_point(_) ->
    ?DEFAULT_DATA_SAMPLE_POINT.
	    

load_color_profile(Profile) ->
    #color {
       layout_background = Profile#profile.layout_background_color,
       layout_border = Profile#profile.layout_border_color,
       background = Profile#profile.frame_background_color,
       foreground = Profile#profile.frame_foreground_color,
       border     = Profile#profile.frame_border_color,
       background1 =Profile#profile.frame_background1_color,
       foreground1 =Profile#profile.frame_foreground1_color
      }.

configure(_Rect,State) ->
    ?verbose("CONFIGURE: ~w\n", [_Rect]),
    State.

key_press(_Event={_, $\t, _Mod, _Code}, _State={S,D}) ->
    D1 = D#d{auto_key = pressed},
    ?verbose("KEY_PRESS: AutoKey pressed\n", []),
    {S,D1};

key_press(_Event={_, $?, _Mod, _Code}, _State={S,D}) ->
    epxw:set_menu(S#s.help_menu),
    ?verbose("KEY_PRESS: help activated\n", []),
    epxw:invalidate(),
    {S, D#d{ help = true }};
key_press(_Event={_, _Sym, _Mod, _Code}, State={_S,_D}) ->
    ?verbose("KEY_PRESS: ~w, mod=~p\n", [_Event, _Mod]),
    State.

key_release(_Event={_, $\e, _Mod, _Code}, _State={S,D}) ->
    ?verbose("KEY_RELEASE: help deactivated\n", []),
    {S, D#d{ help = false }};
key_release(_Event={_, $\t, _Mod, _Code}, _State={S,D}) ->
    D1 = D#d{auto_key = released },
    ?verbose("KEY_RLEASE: AutoKey released\n", []),
    auto_key_detect({S,D1});
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
		    ?verbose("selected layout key=~p bound~p\n", 
			     [Layout#layout.key,
			      {Layout#layout.x, Layout#layout.y,
			       Layout#layout.width,Layout#layout.height}]),
		    cell_hit(XY,Layout,State)
	    end;
	_ ->
	    State
    end.

button_release(_Event={_,_Buttons,{_X,_Y}}, State={S,D}) ->
    ?verbose("BUTTON_RELEASE: ~w\n", [_Event]),
    if D#d.help ->
	    ?verbose("KEY_RELEASE: help deactivated\n", []),
	    {S, D#d{ help = false }};
       true ->
	    State
    end.

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

%%
%% Draw user content
%%
draw(Pixels, Area, State) ->
    ?verbose("DRAW: Area = ~p\n", [Area]),
    redraw_all(Pixels, Area, State),
    State.

%%        +----+-----+       +----+-------+          +------+
%%   Rate |250k|0.800| DRate | 2M | 0.875 | Frames/s |107.5k| Hz
%%        +----+-----+       +----+-------+          +------+

-record(tfield,
	{
	 label = "" :: string(),
	 label_color = ?TEXT_BLACK,   %% black
	 style = none :: none | rect,
	 width = 0 :: integer(),
	 border = 0 :: integer(),
	 border_color = black,
	 align = left :: left|center|right,
	 format = "~s" :: string(),
	 value = "" :: string() | integer() | float(),
	 value_color = ?TEXT_WHITE,   %% white
	 pad = $\s :: char() 
	}).

draw(top, Pixels, _Area, _State={S,D}) ->
    Time1 = erlang:system_time(millisecond),
    Count1 = ets:lookup_element(S#s.frame_counter, all, 2),
    Time0  = D#d.frame_time,
    Count0 = D#d.frame_count,
    FrameRate = 
	case Time1 - Time0 of
	    0 -> 0.0;
	    Td -> 1000*(Count1-Count0)/Td
	end,
    Fs = 
	if is_integer(S#s.bitrate) ->
		[#tfield{label="Rate",
			 label_color=?TEXT_WHITE,
			 style=rect,
			 border=1,
			 border_color=black,
			 format=" ~-8s Hz",
			 value=rate_to_list(S#s.bitrate)}];
	   true ->
		[]
	end ++
	if is_integer(S#s.sample_point) ->
		[#tfield{label="",
			 border=1,
			 border_color=black,
			 format="~.1f%",
			 value=100*(S#s.sample_point / 1000)}];
	   true ->
		[]
	end ++
	if S#s.fd ->
		if is_integer(S#s.datarate) ->
			[#tfield{label="DataRate",
				 label_color=?TEXT_WHITE,
				 style=rect,
				 border=1,
				 border_color=black,
				 format=" ~-8s Hz",
				 value=rate_to_list(S#s.datarate)}];
		   true ->
			[]
		end ++
		if is_integer(S#s.data_sample_point) ->
			[#tfield{label="",
				 border=1,
				 border_color=black,
				 format="~.1f%",
				 value=100*(S#s.data_sample_point/1000)}];
		   true ->
			[]
		end;
	   true ->
		[]
	end ++
	[#tfield{label="FrameRate ",
		 label_color=?TEXT_WHITE,
		 format="~6.1f f/s",
		 style=rect,
		 value=FrameRate}],
    epx_gc:set_font(S#s.top_font),
    X0 = 4,
    Y0 = 4,
    draw_tfields(Pixels,X0,Y0,
		 S#s.top_glyph_width,S#s.top_glyph_ascent,
		 S#s.top_glyph_width,S#s.top_glyph_height,
		 Fs),
    D1 = D#d { frame_count = Count1, frame_time =Time1 },
    State1 = {S, D1},
    State1;
draw(_Where, _Pixels, _Area, State) ->
    State.

draw_tfields(Pixels, X, Y, Xs, Ys, Cw, Ch,
	     [T=#tfield{label=L,width=W,pad=P,format=F,value=V}|Fs]) ->
    epx_gc:set_foreground_color(T#tfield.label_color),
    {X1,_Y1} = epx:draw_string(Pixels,X,Y+Ys,L),
    Value = if F =:= ""; F =:= undefined, is_list(V) ->
		    V;
	       true ->
		    lists:flatten(io_lib:format(F,[V]))
	    end,
    N = if W =:= 0 -> 0;
	   true -> W - length(Value)
	end,
    Value1
	= if N > 0 -> %% pad right/left/center
		  case T#tfield.align of
		      left -> padl(N,P,Value);
		      right -> padr(N,P,Value);
		      center ->
			  NL = N div 2,
			  NR = N - NL,
			  padl(NL,P,padr(NR,P,Value))
		  end;
	     N < 0 -> %% keep field width number of characters
		  lists:sublist(Value, 1, W);
	     true ->
		  Value
	  end,
    epx_gc:set_foreground_color(T#tfield.value_color),
    {X2,_Y2} = epx:draw_string(Pixels,X1,Y+Ys,Value1),
    case T#tfield.style of
	none -> 
	    ok;
	rect ->
	    epx_gc:set_border_color(T#tfield.border_color),
	    epx_gc:set_border_width(T#tfield.border),
	    epx_gc:set_fill_style(none),
	    epx:draw_rectangle(Pixels, X1-1, Y, (X2-X1)+2, Ch),
	    epx_gc:set_border_width(0)
    end,
    draw_tfields(Pixels,X2+Xs,Y,Xs,Ys,Cw,Ch,Fs);
draw_tfields(_Pixels,X,Y,_Xs,_Ys,_Cw,_Ch,[]) ->
    {X,Y}.


padl(0,_Char,Text) -> Text;
padl(I,Char,Text) -> [Char|padl(I-1,Char,Text)].

padr(N,Char,Text) -> 
    Text++lists:duplicate(N,Char).
    

menu({menu,_Pos}, State={S,D}) ->
    if D#d.help ->
	    ?verbose("MENU: help active\n", []),
	    {noreply, State};
       true ->
	    ?verbose("MENU: Pos = ~p\n", [_Pos]),
	    MProfile = S#s.menu_profile,
	    Menu = make_rate_menu(S),
	    CanMenu = epx_menu:create(
			MProfile#menu_profile{background_color=
						  ?RATE_MENU_COLOR},
			Menu),
	    {reply, CanMenu, State}
    end.

make_rate_menu(S) ->
    case S#s.if_can of
	#if_can{if_mod=Mod,if_pid=Pid} when is_pid(Pid) ->
	    [{fd,FD},
	     {bitrate,BitRate}, {bitrates, BitRates0},
	     {datarate,DataRate}, {datarates,DataRates0}] =
		Mod:getopts(Pid, [fd,
				  bitrate, bitrates, 
				  datarate, datarates]),
	    make_bit_rates(BitRate, BitRates0, S) ++
	    make_data_rates(FD, DataRate, DataRates0, S);
	_ ->
	    [{" 125k",    "Alt+1"},
	     {" 500k",    "Alt+2"},
	     {" 250k",    "Alt+3"},
	     {" 1M",      "Alt+4"}
	    ]
    end.

make_bit_rates(BitRate, BitRates0, S) ->
    {BitRate1,BitRates1} = 
	make_rates(BitRate, BitRates0, S#s.conf_bitrates),
    [{rate_to_list(R,BitRate1), "Alt+"++ integer_to_list(I,16)} ||
	{R,I} <- lists:zip(BitRates1,
			   lists:seq(1, length(BitRates1)))].

make_data_rates(true, DataRate, DataRates0, S) ->
    case make_rates(DataRate, DataRates0, S#s.conf_datarates) of
	{_, []} ->
	    [];
	{DataRate1,DataRates1} ->
	    [{"---"}] ++
		[{rate_to_list(R,DataRate1), "Ctrl+"
		  ++integer_to_list(I,16)} ||
		    {R,I} <- lists:zip(DataRates1, 
				       lists:seq(1,
						 length(DataRates1)))]
    end;
make_data_rates(false, _DataRate, _DataRates0, _S) ->
    [].


rate_and_sample_point_to_list(Rate, undefined) ->
    rate_to_list(Rate);    
rate_and_sample_point_to_list(Rate, SamplePoint) ->
    rate_to_list(Rate)++[$@ | percent_to_list(SamplePoint)].


rate_to_list(Rate) ->
    if Rate >= 1000000 ->
	    RateM = Rate div 1000000,
	    RateD = (Rate rem 1000000) div 1000,
	    if RateD =:= 0 ->
		    integer_to_list(RateM)++"M";
	       true ->
		    integer_to_list(RateM)++"."++
			[hd(tl(integer_to_list(1000+RateD))),$M]
	    end;
       Rate >= 1000 ->
	    RateK = Rate div 1000,
	    RateD = Rate rem 1000,
	    if RateD =:= 0 ->
		    integer_to_list(RateK)++"k";
	       true ->
		    integer_to_list(RateK) ++ "." ++
			[hd(tl(integer_to_list(1000+RateD))),$k]
	    end;
       true ->
	    "0"
    end.
		
rate_to_list(Rate,CurrentRate) ->
    if Rate =:= CurrentRate ->
	    ">" ++ rate_to_list(Rate);
       true ->
	    " " ++ rate_to_list(Rate)
    end.

select({_Phase, Rect}, {S,D}) ->
    ?verbose("SELECT: ~w\n", [Rect]),
    {S, D#d { selection = Rect} }.

motion(_Event={motion,_Button,_Pos}, State) ->
    ?verbose("MOTION: ~w\n", [_Event]),
    State.

%%
%% Commands on Selected elements:
%%   B              Binary format
%%   X              Hexadecimal format
%%   D              Decimal format
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
%%   T              Select detected frames and filter out frames with no diff.
%%   F              Filter selected + set filter mode
%%   U              Undelete frames + unset filter mode
%%   
%% Global commands
%%   SPACE          Pause can frame reception / (drop frames)
%%   TAB            Auto key, detect frames the falls during key press
%%   Delete         Remove selected frames
%%   Q              Quit application
%%   R              Refresh screen
%%   Ctrl+K	    Clear/Kill all frames
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
%% Debug
%%   Ctrl+D         Dump internal frame list onto candy.dump
%%
command(Key, Mod, State={_,D}) ->
    ?verbose("COMMAND: key=~p, mod=~p\n", [Key, Mod]),
    try command(Key, D#d.selected, Mod, State) of
	Reply = {reply, _SymMod, _State1} ->
	    Reply;
	State1 ->
	    {noreply, State1}
    catch
	error:Reason:Stack ->
	    io:format("command: ~p\ncrash ~p\n~p\n", 
		      [Key, Reason, Stack]),
	    {stop, Reason, State}
    end.

command($r, _Selected, _Mod, State) ->
    epxw:invalidate(),
    State;
command($k, _Selected, Mod, State) when Mod#keymod.ctrl ->
    epxw:invalidate(),
    clear_frames(State);
command($d, Selected, Mod, State) when Mod#keymod.ctrl ->
    command_dump(Selected, Mod, State);
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
command($h, Selected, Mod, State) ->
    command_sort(Selected, Mod, State);
%% Toggle pid mode
command($p, _Selected, _Mod, {S,D}) ->
    %% toggle pidmode
    NewMode = case S#s.pidmode of
		  off ->  pid24;
		  pid24 -> off
	      end,
    State1 = {S#s { pidmode = NewMode }, D},
    set_status(State1),
    epxw:invalidate(),
    clear_frames(State1);
command($s, Selected, Mod, State) when Mod#keymod.ctrl ->
    command_save(Selected, Mod, State);
command($q, _Selected, Mod, State) when Mod#keymod.ctrl ->
    erlang:halt(0),
    State;

command(I, Selected, _Mod, State) when I >= $1, I =< $8 ->
    Keys = lists:usort([Key || {Key,_} <- Selected]),
    lists:foldl(fun(Key,Si) -> split_fid(Key, Selected, I-$0, Si) end, 
		State, Keys);

command(up, _Selected, Mod, State) when Mod#keymod.shift,Mod#keymod.alt ->
    command_sample_point_next(State);
command(down, _Selected, Mod, State) when Mod#keymod.shift,Mod#keymod.alt ->
    command_sample_point_prev(State);

command(up, _Selected, Mod, State) when Mod#keymod.alt ->
    command_bitrate_prev(State);
command(down, _Selected, Mod, State) when Mod#keymod.alt ->
    command_bitrate_next(State);

command(up, _Selected, Mod, State) when Mod#keymod.shift,Mod#keymod.ctrl ->
    command_data_sample_point_next(State);
command(down, _Selected, Mod, State) when Mod#keymod.shift,Mod#keymod.ctrl ->
    command_data_sample_point_prev(State);

command(up, _Selected, Mod, State) when Mod#keymod.ctrl ->
    command_datarate_prev(State);
command(down, _Selected, Mod, State) when Mod#keymod.ctrl ->
    command_datarate_next(State);

command($l, _Selected, Mod, State) when Mod#keymod.ctrl ->
    command_toggle_listen_only(State);
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
	    epxw:profile_set(screen_color, D#d.orig_screen_color),
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
command($u, _Selected, _Mod, State={_S,D}) ->
    %% show all hidden frames
    Ls = maps:values(D#d.hidden),
    {S1,D1} =
	lists:foldl(
	  fun(L, StX) ->
		  add_unlinked_layout(L, StX)
	  end, State, Ls),
    State2 = {S1#s{ canfilter = false },
	      D1#d{ hidden = #{}, selected = [] }},
    set_status(State2),
    epxw:invalidate(),
    State2;    
    
command($t, _Selected, _Mod, State={S,_}) ->
    Ls =
	fold_layout(
	  fun(L,Acc) ->
		  Key = L#layout.key,
		  case ets:lookup(S#s.frame_diff, Key) of
		      [] -> [{delete,Key}|Acc]; 
		      [{_,[]}] -> [{delete,Key}|Acc];
		      [{_,D}] -> [{select,Key,D}|Acc]
		  end
	  end, [], State),
    {Selected,{S1,D1}} = clear_and_select(Ls,[],State),
    epxw:invalidate(),
    {S1,D1#d{ selected = Selected }};

command($f, Selected, Mod, State) ->
    command_filter(Selected, Mod, State);

command(delete, Selected, Mod, State) ->
    command_delete(Selected, Mod, State);

command($\s, _Selected, _Mod, {S,D}) ->
    Pause = not S#s.pause,
    D1 = if not Pause ->
		 ets:insert(S#s.frame_counter, {all,0}),
		 D#d { frame_count = 0, 
		       frame_time = erlang:system_time(millisecond) };
	    true ->
		 D
	 end,
    State1 = {S#s { pause = Pause }, D1},
    set_status(State1),
    epxw:invalidate(),
    State1;
command(Symbol, _Selected, Mod, State) ->
    io:format("unhandled command ~p\n", [Symbol]),
    {reply,{Symbol,Mod},State}.

%%
%% Command $h sort layouts
%%
command_sort(_Selected, Mod, State = {_S, D}) ->
    Order = D#d.active_order,
    NewOrder = if Mod#keymod.ctrl ->
		       lists:reverse(lists:sort(Order));
		  true ->
		       lists:sort(Order)
	       end,
    State1 = set_order(NewOrder, State),
    update_view(State1),
    epxw:invalidate(),
    maybe_move_restart(State1).

%% active order according to keys
set_order(Keys, _State={S,D}) ->
    {S1,D1} = set_order_(Keys, ?TOP_OFFSET, [], S, D),
    {S1,D1#d { active_order = Keys }}.

set_order_([Key|Keys], Y, Acc, S, D) ->
    L = maps:get(Key, D#d.active),
    Y0 = L#layout.y,
    move_update(Key, Y0-Y, {S, D}),
    L1 = L#layout { y = Y },
    set_order_(Keys, Y+L#layout.height+?LAYOUT_Y_PAD, [{Key,L1}|Acc], S, D);
set_order_([], _Y, Acc, S, D) ->
    {S, D#d { active = maps:from_list(Acc) }}.

%% "delete" all selected frames
command_delete(Selected, _Mod, State) ->
    Ls =
	fold_layout(
	  fun(L,Acc) ->
		  Key = L#layout.key,
		  case lists:member({Key,1}, Selected) of
		      true ->
			  [{delete,Key}|Acc];
		      false -> 
			  Acc
		  end
	  end, [], State),
    {Selected1,{S1,D1}} = clear(Ls,[],State),
    State1 = {S1#s{ canfilter = false}, D1#d{ selected = Selected1 }},
    set_status(State1),
    epxw:invalidate(),
    State1.

%% filter "keep" selected frames
command_filter(Selected, _Mod, State) ->
    Ls =
	fold_layout(
	  fun(L,Acc) ->
		  Key = L#layout.key,
		  case lists:keyfind(Key, 1, Selected) of
		      false -> 
			  [{delete,Key}|Acc];
		      _ ->
			  Acc
		  end
	  end, [], State),
    {_Selected1,{S1,D1}} = clear(Ls,[],State),
    State1 = {S1#s{ canfilter = true}, D1#d{ selected = Selected }},
    set_status(State1),
    epxw:invalidate(),
    State1.

%%
%% Command Ctrl-S save candy file
%%
command_save(Selected, _Mod, State = {S, _D}) ->
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
		format_candy(ID, PID, BitPos, S);
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
    State.    


format_candy(ID, undefined, BitPos, S) ->
    Fd = S#s.fd,
    Bitrate = S#s.bitrate,
    Datarate = S#s.datarate,
    Filter   = S#s.canfilter,
    ["0x",integer_to_list(ID,16),
     " ", integer_to_list(BitPos div 8, 16),  %% hex without 0x (old version)
     %% byte mask
     " 0x", integer_to_list((1 bsl (7-(BitPos rem 8))), 16),
     %% byte match value HIGH
     " 0x", integer_to_list((1 bsl (7-(BitPos rem 8))), 16),
     %% byte match value LOW
     " 0x00", "\n",
     "> canmode=0\n",
     "> canfd=", if Fd -> "1"; true -> "0" end, "\n",
     "> canfilter=", if Filter -> "1"; true -> "0" end, "\n",
     "> bitrate=", integer_to_list(Bitrate), "\n",
     if Fd ->
	     ["> datarate=", integer_to_list(Datarate), "\n"];
	true ->
	     ""
     end,
     "> pidmode=0\n"
    ];
format_candy(ID, PID, BitPos, S) ->
    _FD = S#s.fd,  %% in pid mode FD is always on
    Bitrate = S#s.bitrate,
    Datarate = S#s.datarate,
    Filter   = S#s.canfilter,
    %% PidMode = S#s.pidmode,  only one mode for now
    ["0x",integer_to_list(ID,16),
     " ", integer_to_list(BitPos div 8, 16), %% hex without 0x (old version)
     %% byte mask
     " 0x", integer_to_list((1 bsl (7-(BitPos rem 8))), 16),
     %% byte match value HIGH
     " 0x", integer_to_list((1 bsl (7-(BitPos rem 8))), 16),
     %% byte match value LOW
     " 0x00", "\n",
     "> canmode=0\n",
     "> canfd=1\n",
     "> canfilter=", if Filter -> "1"; true -> "0" end, "\n",
     "> bitrate=", integer_to_list(Bitrate), "\n",
     "> datarate=", integer_to_list(Datarate), "\n",
     "> pidmode=1\n",
     "> pid=0x", tl(integer_to_list(16#1000000+PID,16)), "\n"
    ].

command_dump(_Selected, _Mod, State) ->
    case file:open("candy.dump", [write]) of
	{ok,Fd} ->
	    try dump(Fd, State) of
		ok -> ok
	    after
		file:close(Fd)
	    end,
	    State;
	{error,Reason} ->
	    io:format("unable to dump to candy.dump ~p\n", [Reason]),
	    State
    end.

dump(Fd, State={_S,D}) ->
    lists:foreach(
      fun(Key) ->
	      L = lookup_layout(Key, State),
	      ?verbose("dump layout key=~p, x=~w, y=~w\n", 
		       [L#layout.key, L#layout.x, L#layout.y]),
	      io:put_chars(Fd, [format_layout(L),$.,$\n])
      end, D#d.active_order).

format_layout(L) ->
    format_record(tuple_to_list(L), record_info(fields, layout),0,"  ","\n").

format_record(Vs, Fs) ->
    format_record(Vs, Fs, 0, "", "").

format_record([Name|Vs],Fs,I,IN,NL) ->
    [$#,atom_to_list(Name),${,NL,
     format_fields(Fs,Vs,I,IN,NL),
     indent(I,IN),$}].

format_fields([],[],_I,_IN,_NL) ->
    [];
format_fields([F],[V],I,IN,NL) ->
    [indent(I+1,IN),format_field(F,V,I),NL];
format_fields([F|Fs],[V|Vs],I,IN,NL) ->
    [indent(I+1,IN),format_field(F,V,I),$,,NL |
     format_fields(Fs,Vs,I,IN,NL)].

format_field(F,V,I) ->
    [atom_to_list(F),$=,format_value(F,V,I)].

format_value(_F,R=#color{},I) ->
    format_record(tuple_to_list(R),record_info(fields, color),I+1,"","");
format_value(_F,R=#fmt{},I) ->
    format_record(tuple_to_list(R), record_info(fields, fmt),I+1,"","");
format_value(format,Tuple,I) when is_tuple(Tuple) ->
    [${,$\n,format_value_list(format,tuple_to_list(Tuple),I+1,"  ","\n"), $}];
format_value(key,{ID,undefined},_I) ->
    [${,"16#",candy_record:format_id(ID),",undefined",$}];
format_value(key,{ID,PID},_I) ->
    [${,"16#",candy_record:format_id(ID),",","16#",tl(integer_to_list(16#1000000+PID)),$}];
format_value(_F,V,_I) ->
    io_lib:format("~p",[V]).

%% format v,v...,v
format_value_list(_F,[],_I,_IN,_NL) ->
    [];
format_value_list(F,[V],I,IN,NL) ->
    [indent(I,IN),format_value(F,V,I),NL];
format_value_list(F,[V|Vs],I,IN,NL) ->
    [indent(I,IN),format_value(F,V,I),$,,NL|format_value_list(F,Vs,I,IN,NL)].

indent(0,_) -> [];
indent(I,IN) -> [IN|indent(I-1,IN)].


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

handle_info(_Frame=#can_frame{}, State={S,_D}) when S#s.pause ->
    {noreply, State};    
handle_info(Frame=#can_frame{}, State) ->
    State1 = handle_can_frames(insert_frame(Frame,[],State), State),
    {noreply, State1};
handle_info({timeout,Ref,tick}, State={S,D}) when D#d.tick =:= Ref ->
    case epxw:draw(fun(Pixels,Area) ->
			   State1 = redraw_anim(Pixels,Area,State),
			   epxw:draw_menu_pixels(Pixels),
			   State1
		   end) of
	false ->
	    {noreply, {S, D#d { tick = undefined }}};
	true ->
	    {noreply, tick_start(State)}
    end;

handle_info({timeout,Ref,move}, State={S,D}) when D#d.move =:= Ref ->
    case move_step(State) of
	0 ->
	    %% io:format("move stop\n"),
	    {noreply, {S, D#d { move = undefined }}};
	_ ->
	    epxw:invalidate(),
	    {noreply, move_start(State)}
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
		{BitRate, BitRates} = 
		    make_rates(maps:get(bitrate, IfParam),
			       maps:get(bitrates, IfParam),
			       S#s.conf_bitrates),
		{DataRate,DataRates} = 
		    make_rates(maps:get(datarate, IfParam),
			       maps:get(datarates, IfParam),
			       S#s.conf_datarates),
		SamplePoint0 = maps:get(sample_point, IfParam),
		DataSamplePoint0 = maps:get(data_sample_point, IfParam),
		SamplePoint = sample_point_to_percent(SamplePoint0),
		DataSamplePoint = sample_point_to_percent(DataSamplePoint0),

		Mod = maps:get(mod, IfParam),
		Pid = maps:get(pid, IfParam),
		IFCan = #if_can{if_mod=Mod, if_id = ID, if_pid=Pid},
		S#s { if_state = IfState, 
		      if_can = IFCan,
		      if_param = IfParam,
		      listen_only =LISTEN,
		      fd=FD,
		      bitrate=BitRate, 
		      bitrates=BitRates,
		      datarate=DataRate,
		      datarates=DataRates,
		      sample_point = SamplePoint,
		      data_sample_point = DataSamplePoint
		    };
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

make_rates(Rate, Rates, ConfRates) ->
    Rates1 = ordsets:intersection(sort_rates(Rates), ConfRates),
    Rate1 = select_rate(Rate, Rates1),
    {Rate1, Rates1}.

sort_rates(Rates) ->
    lists:sort(Rates).

%% select Rate if present or next larger
select_rate(undefined, _) -> undefined;
select_rate(Rate, [Rate1|_]) when Rate =< Rate1 -> Rate1;
%%select_rate(Rate, [Rate0,Rate1|_]) when Rate > Rate0, Rate < Rate1 ->
%%    if abs(Rate0-Rate) < abs(Rate1-Rate) -> Rate0;
%%       true -> Rate1
%%    end;
select_rate(_Rate, [Rate1]) ->  Rate1;
select_rate(Rate, [_|Rates]) -> select_rate(Rate, Rates);
select_rate(_Rate, []) -> undefined.

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

-spec framekey(FID::integer(),PID::integer()|undefined) -> framekey().
framekey(FID,PID) ->
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
    ets:update_counter(S#s.frame_counter, all, 1),
    Key = framekey(FID,PID),
    case ets:lookup(S#s.frame, Key) of
	[{_,Frame}] -> %% no change
	    ets:update_counter(S#s.frame_counter, Key, 1),
	    process_frames(Fs, State, Redraw, RedrawSet);
	[{_,OldFrame}] ->
	    ets:insert(S#s.frame, {Key,Frame}),
	    ets:update_counter(S#s.frame_counter, Key, 1),
	    case maps:is_key(Key, D#d.hidden) of
		true ->
		    process_frames(Fs, State, Redraw, RedrawSet);
		false ->
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
		    end
	    end;
	[] ->
	    ets:insert(S#s.frame, {Key,Frame}),
	    case maps:is_key(Key, D#d.hidden) of
		true -> 
		    process_frames(Fs, State, Redraw, RedrawSet);
		false ->
		    State1 = add_frame(FID, PID, Key, State),
		    process_frames(Fs, tick_restart(State1),
				   true, RedrawSet)
	    end
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
		    State1 = 
			maps:fold(
			  fun(Key,_,Statei) ->
				  redraw_by_key(Key, Statei)
			  end, State, RedrawSet),
		    State1
	    end
    end.

%% note that FID is filtered by framekey/1
add_frame(FID,PID,Key,_State={S,D}) ->
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
    L1 = #layout { key=Key, format=Format, color=S#s.color_profile},
    ets:insert(S#s.frame_counter, {Key, 1}),
    ets:insert(S#s.frame_freq, {Key,erlang:system_time(millisecond),1,""}),
    add_layout(L1, {S, D}).


is_auto_detect({_,#d { auto_detect = Detect, auto_key = Key }}) -> 
    (Detect =/= undefined) orelse (Key =:= pressed).

auto_detect_bits(Key, Frame, OldFrame, State={S,_D}) ->
    case is_auto_detect(State) of
	false ->
	    State;  %% bit detection is not running
	true ->
	    case is_auto_edge(State) of
		true -> %% Frame was received inside detection "window"
		    case ets:lookup(S#s.frame_diff,Key) of
			[] -> %% no bits yet, regard all switched bits
			    X1 = diff_bits(Frame,OldFrame),
			    D2 = X1,
			    ets:insert(S#s.frame_diff, {Key,D2}),
			    State;
			[{_,0}] -> %% disabled
			    State;
			[{_,D2}] ->
			    X2 = diff_bits(Frame,OldFrame),
			    D3 = subtract_bits(D2, X2),
			    ets:insert(S#s.frame_diff, {Key,D3}),
			    State
		    end;
		false -> %% NewFrame was received outside detection "window"
		    case ets:lookup(S#s.frame_diff,Key) of
			[] -> %% no bits detected sofar
			    State;
			[{_,0}] -> %% all bits are discarded already
			    State;
			[{_,D2}] ->
			    X2 = diff_bits(Frame,OldFrame),
			    D3 = subtract_bits(D2, X2),
			    ets:insert(S#s.frame_diff, {Key,D3}),
			    State
		    end
	    end
    end.
    
%% return true if auto_detect was switched within edge interval!
is_auto_edge(_State={_,#d{ auto_key = Key }}) when Key =:= pressed ->
    true;
is_auto_edge(_State={_,#d{ auto_detect = Detect, auto_detect_tmr = Tmr}}) 
  when is_reference(Tmr), Detect =/= undefined ->
    case erlang:read_timer(Tmr) of
	false -> false;
	Remain when Detect =:= high -> 
	    T = ?AUTO_DETECT_HIGH_INTERVAL-Remain,
	    (((T < 0) andalso (-T < ?AUTO_DETECT_EDGE_GUESSING)) 
	     orelse
	       ((T >= 0) andalso (T =< ?AUTO_DETECT_EDGE_INTERVAL)));
	Remain when Detect =:= low -> 
	    T = ?AUTO_DETECT_LOW_INTERVAL-Remain,
	    (((T < 0) andalso (-T < ?AUTO_DETECT_EDGE_GUESSING)) 
	     orelse
	       ((T >= 0) andalso (T =< ?AUTO_DETECT_EDGE_INTERVAL)))
    end;
is_auto_edge(_) ->
    false.

clear_and_select(Ls, Sel, St) ->
    {Ls1,St1} = clear(Ls, [], St),
    select(Ls1,Sel,St1).

%% clear delete marked layouts
clear([{delete,Key}|Ls],Acc,St0) ->
    L = lookup_layout(Key, St0),
    St1 = remove_layout(L, St0),
    clear(Ls,Acc,St1);
clear([A={select,_Key,_Bs}|Ls],Acc,St0) ->
    clear(Ls, [A|Acc], St0);
clear([],Acc,St0) ->
    {lists:reverse(Acc),St0}.

%% reposition selected layouts
select([{select,Key,Bs}|Ls],Selected,St0) ->
    L = lookup_layout(Key, St0),
    St1 = unlink_layout(L, St0),
    St2 = add_unlinked_layout(L, St1),
    select(Ls,[{Key,Bs}|Selected], St2);  %% Bs was [?ID_FMT_POSITION]
select([],Selected,St) ->
    {Selected, St}.

%% Filter frames not flipped during key press
auto_key_detect(State = {S,_D}) ->
    Ls = auto_key_detect(ets:first(S#s.frame_diff), S#s.frame_diff, []),
    ets:delete_all_objects(S#s.frame_diff),
    {Selected,{S1,D1}} = clear_and_select(Ls,[],State),
    epxw:invalidate(),
    {S1,D1#d{ selected = Selected }}.

auto_key_detect('$end_of_table', _Tab, Ls) -> 
    Ls;
auto_key_detect(Key, Tab, Ls) -> 
    case ets:lookup(Tab, Key) of
	[] -> %% never flipped
	    auto_key_detect(ets:next(Tab, Key),Tab,[{delete,Key}|Ls]);
	[{_,0}] -> %% flipped
	    auto_key_detect(ets:next(Tab, Key),Tab,[{delete,Key}|Ls]);
	[{_,D2}] -> %% bits flipped (once) during key press
	    auto_key_detect(ets:next(Tab, Key),Tab,[{select,Key,D2}|Ls])
    end.

cell_hit(Xy, Layout, State={S,D}) ->
    IFmt = fmt_from_coord(Xy, Layout), 
    ?verbose("selected i+fmt=~p\n", [IFmt]),
    case IFmt  of
	false ->
	    deselect(State, []);
	{I, _Fmt} ->
	    Mod = epxw:keymod(),
	    if Mod#keymod.shift -> 
		    %% add to or remove from selection
		    Key = Layout#layout.key,
		    Selected = 
			case is_selected({Key,I}, State) of
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

%% is_key_selected(Key, {_S,D}) ->
%%    lists:keymember(Key, 1, D#d.selected).

is_selected(KI={_Key,_I}, {_S,D}) ->
    lists:member(KI, D#d.selected).

command_sample_point_next(State={S,_D}) when  S#s.if_state =:= up ->
    SamplePoint = if S#s.sample_point =:= undefined ->
			  ?MIN_SAMPLE_POINT;
		      true ->
			  S#s.sample_point + ?SAMPLE_POINT_STEP
		  end,
    SamplePoint1 = if SamplePoint > ?MAX_SAMPLE_POINT ->
			   ?MAX_SAMPLE_POINT;
		      true ->
			   SamplePoint
		   end,
    command_bitrate(S#s.bitrate, SamplePoint1, State).

command_sample_point_prev(State={S,_D}) when  S#s.if_state =:= up ->
    SamplePoint = if S#s.sample_point =:= undefined ->
			  undefined;
		      true ->
			  S#s.sample_point - ?SAMPLE_POINT_STEP
		  end,
    SamplePoint1 = if SamplePoint < ?MIN_SAMPLE_POINT ->
			   undefined;
		      true ->
			   SamplePoint
		   end,
    command_bitrate(S#s.bitrate, SamplePoint1, State).
    
command_bitrate_next(State={S,_D}) when S#s.if_state =:= up ->
    I = bitrate_index(State)-1,
    BitRates = S#s.bitrates,
    N = length(BitRates),
    J = ((I+1) rem N) + 1,
    Rate = lists:nth(J, BitRates),
    command_bitrate(Rate, S#s.sample_point, State);
command_bitrate_next(State) ->
    State.

command_bitrate_prev(State={S,_D}) when S#s.if_state =:= up ->
    I = bitrate_index(State)-1,
    BitRates = S#s.bitrates,
    N = length(BitRates),
    J = ((I+(N-1)) rem N) + 1,
    Rate = lists:nth(J, BitRates),
    command_bitrate(Rate, S#s.sample_point, State);
command_bitrate_prev(State) ->
    State.

%% set bitrate and sample_point
command_bitrate(BitRate, SamplePoint, _State={S,D}) ->
    SamplePoint1 = percent_to_sample_point(SamplePoint),
    setopts(S#s.if_can, [{bitrate, BitRate}, {sample_point,SamplePoint1}]),
    [{bitrate,BitRate1},{sample_point,SamplePoint2}] =
	getopts(S#s.if_can, [bitrate, sample_point]),
    SamplePoint3 = sample_point_to_percent(SamplePoint2),
    State1 = {S#s { bitrate = BitRate1, sample_point = SamplePoint3 }, D},
    set_status(State1),
    epxw:invalidate(),
    State1.


bitrate_index({S,_D}) ->
    if S#s.bitrate =:= undefined, S#s.bitrates =:= [] -> 0;
       S#s.bitrate =:= undefined, is_list(S#s.bitrates) -> 1;
       true -> index(S#s.bitrate, S#s.bitrates)
    end.


command_datarate_next(State={S,_D})  when S#s.if_state =:= up, S#s.fd ->
    I = datarate_index(State)-1,
    DataRates = S#s.datarates,
    N = length(DataRates),
    J = ((I+1) rem N) + 1,
    Rate = lists:nth(J, DataRates),
    command_datarate(Rate, S#s.data_sample_point, State);
command_datarate_next(State) ->
    State.

command_datarate_prev(State={S,_D}) when S#s.if_state =:= up, S#s.fd ->
    I = datarate_index(State)-1,
    DataRates = S#s.datarates,
    N = length(DataRates),
    J = ((I+(N-1)) rem N) + 1,
    Rate = lists:nth(J, DataRates),
    command_datarate(Rate, S#s.data_sample_point, State);
command_datarate_prev(State) ->
    State.

command_data_sample_point_next(State={S,_D}) when  S#s.if_state =:= up ->
    SamplePoint = if S#s.data_sample_point =:= undefined ->
			  ?MIN_DATA_SAMPLE_POINT;
		      true ->
			  S#s.data_sample_point + ?SAMPLE_POINT_STEP
		  end,
    SamplePoint1 = if SamplePoint > ?MAX_SAMPLE_POINT ->
			   ?MAX_SAMPLE_POINT;
		      true ->
			   SamplePoint
		   end,
    command_datarate(S#s.datarate, SamplePoint1, State).

command_data_sample_point_prev(State={S,_D}) when  S#s.if_state =:= up ->
    SamplePoint = if S#s.data_sample_point =:= undefined ->
			  undefined;
		      true ->
			  S#s.data_sample_point - ?SAMPLE_POINT_STEP
		  end,
    SamplePoint1 = if SamplePoint < ?MIN_SAMPLE_POINT ->
			   undefined;
		      true ->
			   SamplePoint
		   end,
    command_datarate(S#s.datarate, SamplePoint1, State).

%% set bitrate and sample_point
command_datarate(DataRate, SamplePoint, _State={S,D}) ->
    SamplePoint1 = percent_to_sample_point(SamplePoint),
    _R = setopts(S#s.if_can, [{datarate,DataRate},
			      {data_sample_point,SamplePoint1}]),
    R1 = getopts(S#s.if_can, [datarate, data_sample_point]),
    [{datarate,DataRate1},{data_sample_point,SamplePoint2}] = R1,
    SamplePoint3 = sample_point_to_percent(SamplePoint2),
    State1 = {S#s { datarate = DataRate1,data_sample_point=SamplePoint3}, D},
    set_status(State1),
    epxw:invalidate(),
    State1.


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
command_toggle_listen_only({S,D}) ->
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
    %% State1 = put_layout(L1, State),  %% FIXME: check this
    set_anim(Key, L1, 0, State),
    update_layout(L, L1, State).

%% set animation value for data cells - fixme check fmt + data?
set_anim(Key, Layout, Value, _State={S,_D}) ->
    [ ets:insert(S#s.frame_anim,{{Key,Pos},Value}) ||
	Pos <- lists:seq(9, tuple_size(Layout#layout.format)) ].

%% stop animations on layout be deleteing frame_anim for fields
stop_anim(Layout, State={S,_D}) ->
    Key = Layout#layout.key,
    lists:foreach(fun(I) ->
			  ets:delete(S#s.frame_anim, {Key,I})
		  end, lists:seq(1, tuple_size(Layout#layout.format))),
    State.

%% merge selected data bit fields
merge_fmts(I,[Fmt1,Fmt2|FmtList],Acc,Sel) when 
      Fmt1#fmt.row =:= Fmt2#fmt.row,
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

move_start(_State={S,D}) ->
    %% io:format("move start\n"),
    {S, D#d { move = erlang:start_timer(?MOVE_INTERVAL, self(), move)}}.

move_restart(State={_S,D}) when D#d.move =:= undefined ->
    move_start(State);
move_restart(State) ->
    State.

default_format(IDFmt) ->
    {
     #fmt { field=select, type=undefined },  %% must be position 1
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
     #fmt { field=select, type=undefined },  %% must be position 1
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
     #fmt { row=1,field=data, base=16, type=unsigned, bits=[{64,32}], ci=2},
     #fmt { row=1,field=data, base=16, type=unsigned, bits=[{96,32}], ci=3},

     #fmt { row=2,field=data, base=16, type=unsigned, bits=[{128,32}], ci=4},
     #fmt { row=2,field=data, base=16, type=unsigned, bits=[{160,32}], ci=5},

     #fmt { row=3,field=data, base=16, type=unsigned, bits=[{192,32}], ci=6},
     #fmt { row=3,field=data, base=16, type=unsigned, bits=[{224,32}], ci=7},

     #fmt { row=4,field=data, base=16, type=unsigned, bits=[{256,32}], ci=8},
     #fmt { row=4,field=data, base=16, type=unsigned, bits=[{288,32}], ci=8},

     #fmt { row=5,field=data, base=16, type=unsigned, bits=[{320,32}], ci=9},
     #fmt { row=5,field=data, base=16, type=unsigned, bits=[{352,32}], ci=9},

     #fmt { row=6,field=data, base=16, type=unsigned, bits=[{384,32}], ci=10},
     #fmt { row=6,field=data, base=16, type=unsigned, bits=[{416,32}], ci=10},

     #fmt { row=7,field=data, base=16, type=unsigned, bits=[{448,32}], ci=11},
     #fmt { row=7,field=data, base=16, type=unsigned, bits=[{480,32}], ci=11}
    };
default_fd_format(IDFmt,PIDFmt) ->
    {
     #fmt { field=select, type=undefined },  %% must be position 1
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
    Key = framekey(FID, PID),
    #layout{format=Format} = lookup_layout(Key, State),
    case diff_frames_(1,Format,New,Old,[],State) of
	[] -> [];
	Diff -> Diff
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

%% return the flipped bits between two frames
diff_bits(#frame{data=Data1}, #frame{data=Data2}) ->
    L1 = byte_size(Data1),
    L2 = byte_size(Data2),
    <<D1:L1/little-unit:8>> = Data1,
    <<D2:L2/little-unit:8>> = Data2,
    D1 bxor D2.

%% remove bits from A that are set in B
subtract_bits(A, B) ->
    A band (bnot B).

%% return bit position list
%% bit 0 is the right most bit in the first frame byte
%% 16#1234 -> 16#3412
%% 89AB CDEF 0123 4567
%% 0011 0100 0001 0010
%% [3,6,10,11,13]
-ifdef(not_used).

bit_list(A) -> 
    bit_list(A, 0, 0).

bit_list(0, _, _) -> [];
bit_list(A, I, Pos) when I < 8 ->
    if A band 16#80 =:= 0 ->
	    bit_list(A bsl 1, I+1, Pos+1);
       true ->
	    [Pos|bit_list(A bsl 1, I+1, Pos+1)]
    end;
bit_list(A, 8, Pos) -> 
    bit_list(A bsr 16, 0, Pos).
-endif.


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
%% | Link: up  | Flags: Flags  | Status: busoff |
%% +-----------+---------------+----------------+
set_status(_State={S,_D}) ->
    LinkState = if S#s.pause ->
			"Link: pause";
		   true ->
			case S#s.if_state of
			    up -> "Link: up";
			    down -> "Link: down"
			end
		end,
    LinkFlags =
	"Flags: " ++ 
	lists:join(",", 
		   lists:append(
		     [
		      case S#s.canfilter of
			  true -> ["FILTER"];
			  false -> []
		      end,
		      case S#s.listen_only of 
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
		      end])),
    LinkError =
	case S#s.if_error of 
	    [] -> "State: ok";
	    Es -> "State: "++ format_error(Es)
	end,
    Status = io_lib:format("~-12s ~-20s ~-15s", 
			   [LinkState,LinkFlags,LinkError]),
    epxw:set_status_text(Status),

    AppName = S#s.app_name,
    WindowTitle = case maps:get(device_name, S#s.if_param, "") of
		      "/dev/serial/by-id/usb-LAWICEL_CANUSB_" ++ _ ->
			  AppName++"@CANUSB";
		      "" -> 
			  AppName;
		      DeviceName -> 
			  AppName++"@"++DeviceName
		  end,
    Window = epxw:window(),
    epx:window_adjust(Window, [{name, WindowTitle}]).

sample_point_to_percent(Pt) when is_float(Pt), Pt > 0.0, Pt =< 9.999 ->
    round(1000 * max(0.0, min(Pt, 0.999)));
sample_point_to_percent(undefined) ->
    undefined.

percent_to_sample_point(P) when is_integer(P), P > 0, P < 1000 ->
    P / 1000;
percent_to_sample_point(undefined) ->
    undefined.

percent_to_list(undefined) ->
    "";
percent_to_list(Pt) when is_integer(Pt), Pt >= 0, Pt < 1000 ->
    case integer_to_list(1000 + Pt) of
	[_,$0,B,$0] -> [B,$%];
	[_,$0,B,C] -> [B,$.,C,$%];
	[_,A,B,$0] -> [A,B,$%];
	[_,A,B,C] -> [A,B,$.,C,$%]
    end.

%% float01_to_list(F) when is_float(F), F >= 0.0, F =< 1.0 ->
%%    [$0,$.|integer_to_list(round(F * 1000))].

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
	      State1 =
		  case intersect_layout(Layout, Area) of
		      false ->
			  State;
		      _Intersection ->
			  redraw_layout_(Pixels, Layout, State)
		  end,
	      %% io:format("MENU FROM redraw_by_key\n"),
	      epxw:draw_menu_pixels(Pixels),
	      State1
      end).

%% 
%% Update frame_move  
%%
move_step(_State={S,_D}) ->
    case ets:first(S#s.frame_move) of
	'$end_of_table' ->
	    0;
	Key ->
	    move_step_(S#s.frame_move, Key, 1)
    end.

move_step_(_HTab, '$end_of_table', N) ->
    N;
move_step_(HTab, Key, N) ->
    Next = ets:next(HTab,Key),
    case move_step__(HTab, Key) of
	false ->
	    move_step_(HTab, Next, N);
	true ->
	    move_step_(HTab, Next, N+1)
    end.

%% update each layout 
move_step__(HTab, Key) ->
    case ets:lookup(HTab, Key) of
	[{_,Value,Step}] when Value > 0 ->
	    Value1 = max(0, Value - Step),
	    ets:insert(HTab, {Key,Value1,Step});
	[{_,Value,Step}] when Value < 0 ->
	    Value1 = min(0, Value + Step),
	    ets:insert(HTab, {Key,Value1,Step}),
	    true;
	[{_,_Value,_Step}] ->
	    ets:delete(HTab, Key),
	    false
    end.

%% update move animation delta
move_update(Key, Dy, _State={S,_D}) when Dy /= 0 ->
    N = ?MOVE_TIME_MS div ?MOVE_INTERVAL,
    T = case ets:lookup(S#s.frame_move, Key) of
	    [] ->
		Step = abs(Dy/N),
		{Key, Dy, Step};
	    [{_,Dy0,_}] ->
		Dy1 = Dy0+Dy,
		Step = abs(Dy1/N),
		{Key,Dy1,Step}
	end,
    ets:insert(S#s.frame_move, T);
move_update(_Key, _Dy, _State) ->
    ok.

-spec each_layout(fun((L::layout(), state()) -> state()), S::state()) ->
	  S1::state().
each_layout(Fun, St0={_S,D}) ->
    maps:fold(fun(_,L,Sti) -> Fun(L,Sti) end, St0, D#d.active).

-spec fold_layout(fun((L::layout(), Acc::any()) -> Acc1::any()), 
		  AccIn::any(), S::state()) ->
	  AccOut::any().
fold_layout(Fun, Acc, _State={_S,D}) ->
    maps:fold(fun(_,L,Acc1) -> Fun(L,Acc1) end, Acc, D#d.active).

%% retrive dy while moving/removing frames to get nice animations
layout_dy(Key, _State={S,_D}) ->
    case ets:lookup(S#s.frame_move, Key) of
	[] -> 0;
	[{_,Dy,_}] -> Dy
    end.

layout_y(L, State) ->
    L#layout.y + layout_dy(L#layout.key, State).

redraw_layout_(Pixels, Layout, State={S,_D}) ->
    #layout{ key=Key, color=Color,format=FmtTuple} = Layout,
    draw_layout_background(Pixels, Layout, Layout#layout.color, State),
    [{_,Frame}] = ets:lookup(S#s.frame, Key),

    ?verbose("redraw key=~p, rect=~p\n", 
	     [Key, {Layout#layout.x,Layout#layout.y,
		    Layout#layout.width,Layout#layout.height}]),
    Dy = layout_dy(Key, State),
    redraw_cells(Pixels,
		 Layout#layout.x,Layout#layout.y+Dy,
		 Key,1,Color,FmtTuple,Frame,State).

redraw_cells(Pixels,Lx,Ly,Key,I,Color,FmtTuple,Frame,State) when 
      I =< tuple_size(FmtTuple) ->
    Fmt = element(I, FmtTuple),
    _Remove = redraw_cell(Pixels,Lx,Ly,Key,I,Color,Fmt,Frame,State),
    redraw_cells(Pixels,Lx,Ly,Key,I+1,Color,FmtTuple,Frame,State);
redraw_cells(_Pixels,_Lx,_Ly,_Key,_I,_Color,_FmtTuple,_Frame,State) ->
    State.

redraw_cell(Pixels,Lx,Ly,Key,I,_LayoutColor,Fmt,_Frame,State={_S,_D}) when
      Fmt#fmt.field =:= select ->
    #fmt {x=Dx,y=Dy,width=W,height=H} = Fmt,
    case is_selected({Key,I}, State) of
	true ->
	    epx_gc:set_fill_color(black),
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_border_width(0);
	false ->
	    epx_gc:set_fill_color(white),
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_border_color(black),
	    epx_gc:set_border_width(1)
    end,
    X = Lx+Dx, Y = Ly+Dy,
    epx:draw_ellipse(Pixels, X, Y+2, W-4, H-4),
    epx_gc:set_fill_style(none),
    epx_gc:set_border_width(0),
    false;

redraw_cell(Pixels,Lx,Ly,Key,I,_LayoutColor,Fmt,Frame,State={S,_D}) ->
    #fmt {x=Dx,y=Dy,width=W,height=H,ci=Ci} = Fmt,
    Color = cell_color(Ci),
    X = Lx+Dx, Y = Ly+Dy,
    Anim = get_anim(Key,I,State),
    {Remove,TextColor} = highlight(Pixels,Anim,Color,{X,Y,W,H},State),
    BitsData = get_bits(Fmt, Frame),
    Selected = is_selected({Key,I}, State),
    epx_gc:set_fill_style(none),
    epx_gc:set_foreground_color(Color#color.border),
    epx_gc:set_border_color(Color#color.border),
    epx_gc:set_border_style(inside),
    case Selected of
	true ->
	    epx_gc:set_border_width(?FMT_SELECTED_BORDER_WIDTH);
	_ ->
	    epx_gc:set_border_width(?FMT_BORDER_WIDTH)
    end,
    epx:draw_rectangle(Pixels, {X,Y,W,H}),
    epxw:invalidate({X,Y,W,H}),
    epx_gc:set_border_width(0),

    %% draw base indicator, only for data fields
    if Fmt#fmt.field =:= data ->
	    case Fmt#fmt.base of
		2  ->
		    epx:pixmap_put_pixels(Pixels,
					  X+1,Y+1,6,7,argb,bin_icon(),blend),
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
    Dy = layout_dy(Key, State),
    #layout{ x=Lx, y=Ly, color=Color, format=FmtTuple} = Layout,
    %% fixme add dy to intersect_layout (or ignore while dy > 0)
    case intersect_layout(Layout, Area) of
	false ->
	    step_pos(Key,I,State);
	_Intersection ->
	    redraw_pos_(Pixels,Lx,Ly+Dy,Key,I,Color,FmtTuple,Frame,State)
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
		  layout_background = ?LAYOUT_BACKGROUND_COLOR,
		  layout_border     = ?LAYOUT_BORDER_COLOR,
		  background        = ?TEXT_BACKGROUND_COLOR,
		  foreground        = ?TEXT_COLOR,
		  border            = ?FRAME_BORDER_COLOR,
		  background1       = H,
		  foreground1       = ?TEXT_HIGHLIGHT_COLOR
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

draw_layout_background(Pixels, Layout, Color, State) ->
    draw_layout_rectangle(Pixels, Layout, 
			  Color#color.layout_background,
			  Color#color.layout_border,
			  Layout#layout.width, State).

%% prepare layout by painting the layout background color
clear_layout_background(Pixels, Layout, State={_S,D}) ->
    ScreenColor = D#d.screen_color,
    Color = Layout#layout.color,
    Border = Color#color.layout_border,
    draw_layout_rectangle(Pixels, Layout, ScreenColor, Border,
			  epxw:width(), State).

draw_layout_rectangle(Pixels, Layout, Color, Border, Width, State) ->
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(Color),
    BorderWidth = ?LAYOUT_BORDER_WIDTH,
    epx_gc:set_border_width(BorderWidth),
    epx_gc:set_border_color(Border),
    
    X = Layout#layout.x,
    Y = Layout#layout.y+layout_dy(Layout#layout.key, State),
    W = Width,
    H = Layout#layout.height,
    %% epx:draw_rectangle(Pixels, {X,Y,W,H}).
    epx:draw_roundrect(Pixels, X, Y, W, H, 8, 8),
    epx_gc:set_border_width(0).

glyph_width(S) -> S#s.glyph_width.
%%glyph_height(S) -> S#s.glyph_height.
glyph_ascent(S) -> S#s.glyph_ascent.

get_anim(Key,I,{S,_}) ->
    case ets:lookup(S#s.frame_anim, {Key,I}) of
	[] -> -1;
	[{_,Val}] -> Val
    end.

%% draw hightlight background return text color and flag to signal remove
%% highlight(_Pixels,-1, Color, _Rect, _State) ->
%%    {true, Color#color.foreground};
highlight(Pixels,Anim, Color, Rect, _State) when Anim =< 0 ->
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
get_bits(#fmt { field=select }, _Frame) -> 
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

%% clear is like remove but without moving, remove from active
%% move to hidden etc
clear_layout(Layout, State) ->
    #layout { key=Key } = Layout,
    {S1,D1} = stop_anim(Layout, State),
    Order  = D1#d.active_order -- [Key],
    Active = maps:remove(Key, D1#d.active),
    Hidden = maps:put(Key, Layout#layout{x=0,y=0},D1#d.hidden),
    {S1, D1#d{active=Active,hidden=Hidden,active_order=Order}}.

%% Layout is to be "delete", 
%% reposition layouts below the Layout, update next_pos
%% update view_height / view_width

%% move hidden frame layouts hidden map
remove_layout(Layout, State) ->
    #layout { height=H, y=Y } = Layout,
    State1 = move_layout_up(Y, H+?FMT_VERTICAL_PAD, State),
    State2 = clear_layout(Layout, State1),
    maybe_move_restart(State2).

maybe_move_restart(State={S,_D}) ->
    case ets:info(S#s.frame_move, size) of
	0 -> State;
	_ -> move_restart(State)
    end.

%% Move all layouts below Y up by H pixels
move_layout_up(Y, H, State={_S,_D}) ->
    %% first collect Frame layouts below Y
    Keys = fold_layout(
	     fun(L, Acc) ->
		     Y1 = L#layout.y, %% layout_y(L, State),
		     if Y1 > Y ->
			     [L#layout.key|Acc];
			true ->
			     Acc
		     end
	     end, [], State),
    %% Now move the frames H pixels up
    lists:foldl(
      fun(Key, State1) ->
	      L = lookup_layout(Key, State1),
	      move_update(Key, H, State1),
	      L1 = L#layout { y = L#layout.y-H },
	      put_layout(L1, State1)
      end, State, Keys).

%% calculate new view rect
%% view_rect(State) ->
%%     fold_layout(fun(L1, Ri) ->
%%			R = {L1#layout.x,L1#layout.y,
%%			     L1#layout.width,L1#layout.height},
%%			epx_rect:union(Ri, R)
%%		end, {0,0,0,0}, State).

next_xy({_S,D}) ->
    case D#d.active_order of
	[] ->
	    {?LEFT_OFFSET, ?TOP_OFFSET};
	Order ->
	    Key = lists:last(Order),
	    L = maps:get(Key, D#d.active),
	    {L#layout.x,
	     L#layout.y + L#layout.height + ?LAYOUT_Y_PAD}
    end.

position_cells(FmtTuple, State) ->
    %%Dx=?LAYOUT_X_PAD, Dy=?LAYOUT_Y_PAD,
    Dx=0, Dy=0,
    Row=0,
    position_cells(1,Dx,Dy,Row,0,FmtTuple,[],State).

position_cells(I,Dx,Dy,Row,MaxW,FmtTuple,Acc,State={S,_D}) 
  when I =< tuple_size(FmtTuple) ->
    Fmt = element(I, FmtTuple),
    Width = fmt_width(Fmt, S), 
    Height = S#s.row_height,
    if Row =:= Fmt#fmt.row ->
	    Fmt1 = Fmt#fmt { x=Dx+?LAYOUT_X_PAD,
			     y=Dy+?LAYOUT_Y_PAD,
			     width=Width, height=Height },
	    position_cells(I+1,Dx+Width+2,Dy,Row,MaxW,FmtTuple,
			   [Fmt1|Acc],State);
       true ->
	    MaxW1 = max(MaxW,Dx-2),
	    Dy1 = Dy + Height + ?FMT_VERTICAL_PAD,
	    %% FIXME: calculate once! assumes fmt 2-8 are on same row!
	    Dx1 = fmt_width(element(1,FmtTuple),S) +
		       fmt_width(element(2,FmtTuple),S) + 
		       fmt_width(element(3,FmtTuple),S) + 
		       fmt_width(element(4,FmtTuple),S) + 
		       fmt_width(element(5,FmtTuple),S) + 
		       fmt_width(element(6,FmtTuple),S) + 
		       fmt_width(element(7,FmtTuple),S) + 
		       fmt_width(element(8,FmtTuple),S) + 2*8,
	    Fmt1 = Fmt#fmt { x=Dx1+?LAYOUT_X_PAD, 
			     y=Dy1+?LAYOUT_Y_PAD,
			     width=Width, height=Height },
	    position_cells(I+1,Dx1+Width+2,Dy1,Fmt#fmt.row,MaxW1,
			   FmtTuple,[Fmt1|Acc],
			   State)
    end;
position_cells(I,Dx,Dy,_Row,MaxW,_FmtTuple,Acc,_State={S,_D}) ->
    FmtTuple1 = list_to_tuple(lists:reverse(Acc)),
    if I =:= 1 -> {0,0,FmtTuple1,0};
       true -> {?LAYOUT_X_PAD+max(MaxW,Dx-2)+?LAYOUT_X_PAD,
		?LAYOUT_Y_PAD+Dy+S#s.row_height+?LAYOUT_Y_PAD,
		FmtTuple1}
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
	{string,String} -> %% string
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
clear_frames(_State={S,D}) ->
    ets:delete_all_objects(S#s.frame),
    %% ets:delete_all_objects(S#s.frame_layout),
    ets:delete_all_objects(S#s.frame_anim),
    ets:delete_all_objects(S#s.frame_move),
    ets:delete_all_objects(S#s.frame_freq),
    ets:delete_all_objects(S#s.frame_counter),
    ets:insert(S#s.frame_counter, {all,0}),
    {S, D#d{ active=#{}, hidden=#{}, active_order=[],
	     frame_count = 0,
	     frame_time = erlang:system_time(millisecond)
	   }}.

%% FID maybe frame id or internal key (where fd/err is stripped)
%% lookup_layout(Key,_State={S,_D}) ->
%%    [{_,L}] = ets:lookup(S#s.frame_layout,Key),
%%    L.
-spec lookup_layout(Key::framekey(), S::state()) -> layout().
lookup_layout(Key, {_S,D}) ->
    maps:get(Key, D#d.active).

%% update existing layout, may change width
update_layout(OldLayout, NewLayout, State={_S,_D}) ->
    %% wrap with epxw to be able to redraw old layout
    epxw:draw(
      fun(Pixels, _Area) ->
	      clear_layout_background(Pixels, OldLayout, State),
	      {Width,Height,FmtTuple} = 
		  position_cells(NewLayout#layout.format, State),
	      %% fixme: check that only width may be changed!
	      Layout = NewLayout#layout { width=Width,height=Height,
					  format=FmtTuple },
	      State1 = put_layout(Layout, State),
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

%% add "new" layout last, recalculate all fields
add_layout(L, State) ->
    {X,Y} = next_xy(State),
    {Width,Height,FmtTuple} = position_cells(L#layout.format, State),
    L1 = L#layout { x=X,y=Y,width=Width,height=Height,format=FmtTuple },    
    State1 = add_order(L1#layout.key, State),
    put_layout(L1, State1).

%% re-add an "unlinked" layout last
add_unlinked_layout(L, State) ->
    %% Pos = next_pos(State),
    {X,Y} = next_xy(State),
    %%L1 = L#layout { pos=Pos,x=X,y=Y },
    L1 = L#layout { x=X,y=Y },
    State1 = add_order(L1#layout.key, State),
    put_layout(L1, State1).

put_layout(L, _State={S,D}) ->
     %% FIXME: use smarter calculation!
    {_,_,W,H} = epxw:view_rect(),
    epxw:set_view_size(max(W,L#layout.x+L#layout.width),
 		       max(H,L#layout.y+L#layout.height)),
    Active = maps:put(L#layout.key, L, D#d.active),
    {S, D#d { active = Active }}.

update_view(_State={_S,D}) ->
    case D#d.active_order of
	[] ->
	    epxw:set_view_size(0, 0);
	Order ->
	    Key = lists:last(Order),
	    L = maps:get(Key, D#d.active),
	    {_,_,W,_H} = epxw:view_rect(),
	    epxw:set_view_size(max(W,
				   L#layout.x+L#layout.width+?RIGHT_OFFSET),
			       L#layout.y+L#layout.height+?BOTTOM_OFFSET)
    end.

add_order(Key, _State={S, D}) ->
    {S, D#d{ active_order = D#d.active_order ++ [Key] }}.

%% Like clear layout but do not put on hidden map
%% remove from active and active_order list
unlink_layout(Layout, State) ->
    #layout { key=Key } = Layout,
    {S1,D1} = stop_anim(Layout, State),
    Order  = D1#d.active_order -- [Key],
    Active = maps:remove(Key, D1#d.active),
    {S1, D1#d{active=Active,active_order=Order}}.

layout_from_coord(XY, {_, D}) ->
    layout_from_coord_(XY, D#d.active_order, D#d.active).

layout_from_coord_(XY={X,Y}, [Key|Keys], Layouts) ->
    L = maps:get(Key, Layouts),
    Y1 = L#layout.y,
    Y2 = Y1 + L#layout.height,
    X1 = L#layout.x,
    X2 = X1 + L#layout.width,
    if Y =< Y2, Y >= Y1, X >= X1, X =< X2 -> L;
       Y < Y2 -> false;
       true -> layout_from_coord_(XY, Keys, Layouts)
    end;
layout_from_coord_(_, [], _Layouts) ->
    false.

fmt_from_coord(Xy, Layout) ->
    fmt_from_coord_(Xy, 1, Layout#layout.x,Layout#layout.y, 
		    Layout#layout.format).

fmt_from_coord_(Xy={X,Y}, I, Lx, Ly, FmtTuple) when I =< tuple_size(FmtTuple) ->
    F = element(I, FmtTuple),
    #fmt{x=Dx,y=Dy,width=W,height=H} = F,
    ?verbose("~w: xy=~p, field_pos =~p, ~p\n",
	     [I, Xy, {Lx+Dx,Ly+Dy}, {Lx+Dx+W, Ly+Dy+H}]),
    if 
	X >= (Lx+Dx), Y >= (Ly+Dy), X < Lx+Dx+W, Y < Ly+Dy+H ->    
	    {I, F};
	true ->
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

%%
%% GET and SET can bitrate on the can backend
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
    IFs = can_router:interfaces(),
    get_can_if_(IFs).

%% fixme export can_if from can_router!?
get_can_if_(IFs) ->
    get_can_if_(IFs, undefined).

get_can_if_([#can_if { mod=can_usb,pid=IfPid,id=IfID, 
		       state=up } | _IFs], _) ->
    {ok,#if_can{if_mod=can_usb,if_id=IfID,if_pid=IfPid}};

get_can_if_([#can_if { mod=can_sock,pid=IfPid,id=IfID,
		       state=up } | _IFs], _) ->
    {ok,#if_can{if_mod=can_sock,if_id=IfID,if_pid=IfPid}};

get_can_if_([#can_if { mod=Mod,pid=IfPid,id=IfID,
		       state=up } | IFs], _) ->
    get_can_if_(IFs, #if_can{if_mod=Mod,if_id=IfID,if_pid=IfPid});

get_can_if_([#can_if{mod=Mod,pid=IfPid,id=IfID}|IFs], 
	    undefined) ->
    get_can_if_(IFs, #if_can{if_mod=Mod,if_id=IfID,if_pid=IfPid});

get_can_if_([_|IFs], Default) ->
    get_can_if_(IFs, Default);
get_can_if_([], undefined) ->
    {error, no_backend_found};
get_can_if_([], Default) ->
    {ok, Default}.
