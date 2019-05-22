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

-behaviour(gen_server).

%% API
-export([start/0, start/1]).
-export([status/0]).
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-import(lists, [foldl/3]).

-include_lib("can/include/can.hrl").

-type unsigned() :: non_neg_integer().

-compile(export_all).

-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(GET_STACK(Stacktrace), Stacktrace).
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(GET_STACK(_), erlang:get_stacktrace()).
-endif.

-define(FONT_SIZE, 14).
-define(BACKGROUND_COLOR,        {0,255,255}).     %% cyan
-define(LAYOUT_BACKGROUND_COLOR, {255,255,255}).   %% white
-define(HIGHLIGHT_COLOR,         {255,0,0}).       %% red hight light
-define(FRAME_BORDER_COLOR,      {0,0,0}).         %% black border
-define(TEXT_COLOR,              {0,0,0,0}).       %% black text
-define(TEXT_HIGHLIGHT_COLOR,    {0,255,255,255}). %% white hight light
-define(SCROLL_BAR_SIZE,         16).
-define(SCROLL_BAR_COLOR,        {240,240,240}).
-define(SCROLL_HNDL_SIZE,        10).
-define(SCROLL_HNDL_COLOR,       {127,127,127}).
-define(SCROLL_HORIZONTAL,       bottom).
-define(SCROLL_VERTICAL,         right).

-define(XSTEP, 4).
-define(YSTEP, 4).

-record(fmt,
	{
	 x :: non_neg_integer(),
	 y :: non_neg_integer(),
	 width :: non_neg_integer(),
	 height :: non_neg_integer(),
	 hidden = false :: boolean(),
	 type = unsigned :: unsigned | signed | undefined |
			    {enum,tuple()} | {string,string()},
	 field = none :: none | id | len | data | frequency | hide,
	 base = 16 :: 0 | 2 | 8 | 16 | 10,
	 signed = false :: boolean(),
	 bits = [] :: [{Pos::non_neg_integer(),Length::non_neg_integer()}]
	}).

%% can frame color
-record(color,
	{
	 background  = ?LAYOUT_BACKGROUND_COLOR,
	 foreground  = ?TEXT_COLOR,               %% text color
	 border      = ?FRAME_BORDER_COLOR,       %% frame border color
	 background1  = ?HIGHLIGHT_COLOR,
	 foreground1  = ?TEXT_HIGHLIGHT_COLOR
	}).

-record(layout,
	{
	 id  :: integer(),             %% frame id
	 pos :: integer(),             %% list position
	 style = normal :: normal | fixed | collapsed | hidden | deleted,
	 color = #color{} :: #color{}, %% color profile
	 x     :: integer(),           %% x offset
	 y     :: integer(),           %% y offset
	 width :: non_neg_integer(),   %% total width
	 height :: non_neg_integer(),  %% total height
	 format = {} :: tuple()        %% {#fmt{},...}
	}).

-define(ID_FMT_POSITION, 2).

%% kind of constant elements
-record(s,
	{
	 width  :: integer(),
	 height :: integer(),
	 nrows = 30 :: integer(),     %% number or rows shown
	 window :: epx:epx_window(),  %% attached window
	 font   :: epx:epx_font(),
	 %% foreground_pixels :: epx:epx_pixmap(),
	 pixels :: epx:epx_pixmap(),
	 frame,          %% ets: #can_frame{}
	 frame_layout,   %% ets: #layout{}
	 frame_counter,  %% ets: ID -> Counter
	 frame_freq,     %% ets: {ID,Time,OldCounter}
	 frame_anim,     %% ets: {ID,FmtPos} -> Counter
	 glyph_width   :: unsigned(),
	 glyph_height  :: unsigned(),
	 glyph_ascent  :: integer(),
	 background_color :: epx:epx_color(),
	 top_offset    = 5,
	 left_offset   = 5,
	 right_offset  = 5,
	 bottom_offset = 5,
	 row_width     = 0,
	 row_height    = 0,
	 row_pad = 3,
	 next_pos = 1,
	 hide :: epx:epx_pixmap()
	}).

%% dynamic state elements
-record(d,
	{
	 alt    = false,
	 ctrl   = false,
	 shift  = false,
	 tick :: undefined | reference(),
	 selected = [],
	 view_xpos = 0,   %% scroll x
	 view_ypos = 0,   %% scroll y
	 view_left = 0    :: integer(),
	 view_right = 0   :: integer(),
	 view_top = 0     :: integer(),
	 view_bottom = 0  :: integer(),
	 hscroll          :: undefined | epx:epx_rect(),
	 hhndl            :: undefined | epx:epx_rect(),
	 vscroll          :: undefined | epx:epx_rect(),
	 vhndl            :: undefined | epx:epx_rect(),
	 motion           :: undefined |
			     {vhndl,epx:epx_point()} |
			     {hhndl,epx:epx_point()}
	}).

%%
%% gen server state is divided in a "static" part
%% and one dynamic part
%%  {#s{}, #d{}}
%%

status() ->
    io:format("candy is running\n").

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    start(any).
start(Model) ->
    (catch error_logger:tty(true)),
    application:ensure_all_started(?MODULE),
    can_udp:start(),
    gen_server:start(?MODULE, [Model], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link(any).
start_link(Model) ->
    (catch error_logger:tty(false)),
    application:ensure_all_started(?MODULE),
    can_udp:start(),
    gen_server:start_link(?MODULE, [Model], []).

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
init([Model]) ->
    process_flag(trap_exit, true),
    can_router:attach(),
    {ok,Font} = epx_font:match([{name,"Courier New"},{size,?FONT_SIZE}]),
    epx_gc:set_font(Font),
    S0 = #s{},
    {W,H}  = epx_font:dimension(Font,"0"),
    RowHeight = H + 2,
    NRows = S0#s.nrows + 2,  %% 2 collapsed frame rows
    Height = S0#s.top_offset+NRows*(RowHeight+S0#s.row_pad)
	- S0#s.row_pad + S0#s.bottom_offset,
    %% FORMAT= ID X|R|E L 01 23 45 67 01 23 45 67
    %% maximum width = 
    %%   ID: 3FF (11-bit) | 1FFFFFFF (29-bit)
    %% Data: 64 bit : 8*8 (16 char) | 64*1 (64 char) | X|R
    %% Given 8 byte groups in base 2
    Width = S0#s.left_offset + 
	1*(8*W+4+2) + 3*(1*W+4+2) + 1*(1*W+4+2) + 8*(6+8*W+4+2) - 2 +
	S0#s.right_offset,
    Window = epx:window_create(40, 40, Width, Height,
			       [button_press,button_release,
				key_press, key_release, configure]),
    Bg = epx:pixmap_create(Width, Height, argb),
    epx:pixmap_fill(Bg, ?BACKGROUND_COLOR),
    epx:window_attach(Window),
    epx:pixmap_attach(Bg),
    epx:window_adjust(Window, [{name, "Candy"}]),
    S1 = S0#s {
	   width = Width,
	   height = Height,
	   window = Window,
	   font   = Font,
	   glyph_width  = W,
	   glyph_height = H,
	   glyph_ascent = epx:font_info(Font, ascent),
	   background_color = ?BACKGROUND_COLOR,
	   %% foreground_pixels = Fg,
	   pixels = Bg,
	   frame = ets:new(frame, [{keypos,#can_frame.id}]),
	   frame_layout  = ets:new(frame_layout, [{keypos,#layout.id}]),
	   frame_counter = ets:new(frame_counter, []),
	   frame_anim    = ets:new(frame_anim, []),
	   frame_freq    = ets:new(frame_freq, []),
	   row_width = Width,
	   row_height = RowHeight,
	   hide = hide_pixels()
	  },
    D = #d { },
    State = {S1, D},
    State1 = load_frame_layout(Model, State),
    {ok, update_window(State1)}.

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
	   #fmt { field=id,base=0,type={enum,{"-","X"}},bits=[{0,1}]},
	   #fmt { field=id,base=0,type={enum,{"-","R"}},bits=[{1,1}]},
	   #fmt { field=id,base=0,type={enum,{"-","E"}},bits=[{2,1}]},
	   #fmt { field=len,base=16,type=unsigned,bits=[{0,4}]} |
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
    redraw(FID, State2).

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
    Reply = ok,
    {reply, Reply, State}.

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

handle_info(Frame=#can_frame{id=FID}, State={S,D}) ->
    case ets:lookup(S#s.frame, FID) of
	[Frame] -> %% no change
	    ets:update_counter(S#s.frame_counter, FID, 1),
	    {noreply, State};
	[Frame0] ->
	    ets:insert(S#s.frame, Frame),
	    ets:update_counter(S#s.frame_counter, FID, 1),
	    case diff_frames(FID,Frame,Frame0,State) of
		[] ->
		    {noreply, tick_restart(State)};
		Diff ->
		    [ ets:insert(S#s.frame_anim,{{FID,Pos},255})|| Pos <- Diff],
		    redraw(FID, State),
		    {noreply, tick_restart(State)}
	    end;
	[] ->
	    ets:insert(S#s.frame, Frame),
	    IDFmt = 
		if FID band ?CAN_EFF_FLAG =/= 0 ->
			#fmt {field=id,base=16,type=unsigned,bits=[{3,29}]};
		   true ->
			#fmt {field=id,base=16,type=unsigned,bits=[{21,11}]}
		end,
	    Format = default_format(IDFmt),
	    [ ets:insert(S#s.frame_anim,{{FID,Pos},255}) ||
		Pos <- lists:seq(1,tuple_size(Format))  ],

	    LPos = S#s.next_pos,
	    Layout1 = #layout { id=FID, pos=LPos, format=Format },
	    Layout2 = position_layout(Layout1, State),

	    State1 = {S#s{ next_pos = LPos+1}, D},
	    State2 = insert_layout(Layout2, State1),

	    ets:insert(S#s.frame_counter, {FID, 1}),
	    ets:insert(S#s.frame_freq, {FID,erlang:system_time(millisecond),1,""}),
	    %% SaveClip = clip_window_content(State1),
	    %% State2 = redraw_layout_(Layout2, State1),
	    %% set_clip_rect(State2, SaveClip),
	    %% State3 = repaint_layout(Layout2, State2, 0),
	    State3 = redraw(State2),
	    {noreply, tick_restart(State3)}
    end;
handle_info({timeout,Ref, tick}, State={S,D}) when D#d.tick =:= Ref ->
    Save = clip_window_content(State),
    R = redraw_anim(State),
    set_clip_rect(State, Save),
    case R of
	false ->
	    {noreply, {S, D#d { tick = undefined }}};
	true ->
	    State1 = update_window(State),
	    {noreply, tick_start(State1)}
    end;

handle_info({epx_event, Win, Event}, State={S,_}) when S#s.window =:= Win ->
    try epx_event(Event, State) of
	Return -> Return
    catch
    ?EXCEPTION(error,Reason,Stack) ->
	    io:format("crash: ~p\n", [Reason]),
	    io:format("~p\n", [?GET_STACK(Stack)]),
	    {noreply, State}
    end;

handle_info(_Info, State) ->
    io:format("Got info ~p\n", [_Info]),
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
terminate(_Reason, {S,_D}) ->
    epx:pixmap_detach(S#s.pixels),
    epx:window_detach(S#s.window),
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


epx_event(close, State={_S,_D}) ->
    %% io:format("Got window close\n", []),
    erlang:halt(0),   %% temporary hack
    {stop, normal, State};
epx_event({button_press,[left],{X0,Y0,_}}, State={_S,D}) ->
    case scroll_hit({X0,Y0}, State) of
	false ->
	    Pos = {X0+D#d.view_xpos, Y0+ D#d.view_ypos},
	    case layout_from_position(Pos, State) of
		false ->
		    {noreply, deselect(State, [])};
		Layout ->
		    {noreply, cell_hit(Pos,Layout,State)}
	    end;
	State1 ->
	    {noreply, State1}
    end;

epx_event({button_release,[left],{_X,_Y,_}}, State={S,D}) ->
    flush_motion(S#s.window),
    case D#d.motion of
	undefined ->
	    {noreply, State};
	{vhndl,_Delta} ->
	    epx:window_disable_events(S#s.window, [motion]),
	    {noreply, {S,D#d { motion = undefined }}};
	{hhndl,_Delta} ->
	    epx:window_disable_events(S#s.window, [motion]),
	    {noreply, {S,D#d { motion = undefined }}}
    end;

epx_event({motion,_Button,{X1,Y1,_}},State={S,D}) ->
    flush_motion(S#s.window),
    case D#d.motion of
	{vhndl,{_DX,Dy}} ->
	    {_,_,_,H} = D#d.vscroll,
	    VH = (D#d.view_bottom - D#d.view_top),
	    Y2  = trunc((Y1-Dy)*(VH/H)),
	    B = max(0,(D#d.view_bottom + S#s.bottom_offset)-H),
	    Y  = if Y2 < 0 -> 0;
		    Y2 > B -> B;
		    true -> Y2
		 end,
	    State1 = {S, D#d { view_ypos = Y }},
	    {noreply,redraw(State1)};
	{hhndl,{Dx,_Dy}} ->
	    {_,_,W,_} = D#d.hscroll,
	    VW = (D#d.view_right - D#d.view_left),
	    X2  = trunc((X1-Dx)*(VW/W)),
	    R = max(0, (D#d.view_right-S#s.right_offset)-W),
	    X = if X2 < 0 -> 0;
		   X2 > R -> R;
		   true -> X2
		end,
	    State1 = {S, D#d { view_xpos = X }},
	    {noreply,redraw(State1)}; 
	_ ->
	    {noreply,State}
    end;

epx_event({button_press,[wheel_down],{_X,_Y,_}},State) ->
    {noreply, State};
epx_event({button_release,[wheel_down],{_X,_Y,_}},State={S,_D}) ->
    flush_wheel(S#s.window),
    {noreply, scroll_down(State)};

epx_event({button_press,[wheel_up],{_X,_Y,_}},State) ->
    {noreply, State};
epx_event({button_release,[wheel_up],{_X,_Y,_}},State={S,_D}) ->
    flush_wheel(S#s.window),
    {noreply, scroll_up(State)};

epx_event({button_press,[wheel_left],{_X,_Y,_}}, State) ->
    {noreply, State};
epx_event({button_release,[wheel_left],{_X,_Y,_}},State={S,_D}) ->
    flush_wheel(S#s.window),
    {noreply, scroll_left(State)};

epx_event({button_press,[wheel_right],{_X,_Y,_}}, State) ->
    {noreply, State};    
epx_event({button_release,[wheel_right],{_X,_Y,_}},State={S,_D}) ->
    flush_wheel(S#s.window),
    {noreply, scroll_right(State)};

epx_event({key_press, Sym, Mod, _Code}, _State={S,D}) ->
    Shift = case lists:member(shift,Mod) of
		true -> true;
		false -> D#d.shift
	    end,
    Ctrl = case lists:member(ctrl,Mod) of
	       true -> true;
	       false -> D#d.ctrl
	   end,
    Alt = case lists:member(alt,Mod) of
	      true -> true;
	      false -> D#d.alt				   
	  end,
    D1 = D#d { shift=Shift, ctrl=Ctrl, alt=Alt},
    State2 = command(Sym, D#d.selected, {S,D1}),
    {noreply, State2};    

epx_event({key_release, _Sym, Mod, _code},_State={S,D}) ->
    %% %% io:format("Key release ~p mod=~p\n", [_Sym,Mod]),
    Shift = case lists:member(shift,Mod) of
		true -> false;
		false -> D#d.shift
	    end,
    Ctrl = case lists:member(ctrl,Mod) of
	       true -> false;
	       false -> D#d.ctrl
	   end,
    Alt = case lists:member(alt,Mod) of
	      true -> false;
	      false -> D#d.alt
	  end,
    D1 = D#d { shift = Shift, ctrl = Ctrl, alt = Alt },
    {noreply, {S,D1}};

epx_event({configure, Rect},_State={S,D}) ->
    {_X,_Y,W,H} = flush_configure(S#s.window, Rect),
    Pixels = resize_pixmap(S#s.pixels, W, H),
    State1 = {S#s { width=W, height=H,  pixels=Pixels }, D},
    redraw(State1),
    {noreply, State1};
epx_event(Event, State) ->
    io:format("Got epx event ~p\n", [Event]),
    {noreply, State}.

scroll_hit(Pos, _State={S,D}) ->
    case epx_rect:contains(D#d.hscroll, Pos) of
	true ->
	    epx:window_enable_events(S#s.window, [motion]),
	    case epx_rect:contains(D#d.hhndl, Pos) of
		true ->
		    {Xv,Yv,_,_} = D#d.hhndl,
		    {Xp,Yp} = Pos,
		    Delta = {Xp-Xv, Yp-Yv},
		    {S,D#d{motion={hhndl,Delta}}};
		false ->
		    {_,_,W,_} = D#d.hscroll,
		    {_,_,Vw,_} = D#d.vhndl,
		    {Xp,_Yp} = Pos,
		    VW = (D#d.view_right - D#d.view_left),
		    X2  = trunc((Xp-(Vw div 2))*(VW/W)),
		    R = max(0, (D#d.view_right-S#s.right_offset)-W),
		    X  = clamp(X2, 0, R),
		    State1 = {S, D#d { view_xpos = X, 
				       motion={hhndl,{(Vw div 2),0}} }},
		    redraw(State1)
	    end;
	false ->
	    case epx_rect:contains(D#d.vscroll, Pos) of
		true ->
		    epx:window_enable_events(S#s.window, [motion]),
		    case epx_rect:contains(D#d.vhndl, Pos) of
			true ->
			    {Xv,Yv,_,_} = D#d.vhndl,
			    {Xp,Yp} = Pos,
			    Delta = {Xp-Xv, Yp-Yv},
			    %% activate motion
			    {S,D#d{motion={vhndl,Delta}}};
			false ->
			    {_,_,_,H} = D#d.vscroll,
			    {_,_,_,Vh} = D#d.vhndl,
			    {_Xp,Yp} = Pos,
			    VH = (D#d.view_bottom - D#d.view_top),
			    Y2  = trunc((Yp-(Vh div 2))*(VH/H)),
			    B = max(0,(D#d.view_bottom+S#s.bottom_offset)-H),
			    Y  = clamp(Y2, 0, B),
			    State1 = {S, D#d { view_ypos = Y, 
					       motion={vhndl,{0,(Vh div 2)}}
					     }},
			    redraw(State1)
		    end;
		false ->
		    false
	    end
    end.

clamp(Value, Min, Max) ->
    if Value < Min -> Min;
       Value > Max -> Max;
       true -> Value
    end.

cell_hit(Pos, Layout, State={S,D}) ->
    case fmt_from_position(Pos, Layout) of
	false ->
	    deselect(State, []);
	{_I, Fmt} when Fmt#fmt.field =:= hide ->
	    {Layout1,State1} = remove_layout(Layout, State),
	    %% Note that Layout1 is the "delete" marked Layout
	    State2 = insert_layout(Layout1, State1),
	    {Vx,Vy,Vw,Vh} = view_rect(State2),
	    {S2,D2} = State2,
	    D3 = D#d { view_left = Vx, view_top = Vy,
		       view_right = Vx+Vw-1,
		       view_bottom = Vy+Vh-1 },
	    State3 = {S2,D3},
	    redraw(State3);
	    
	{_I, Fmt} when Fmt#fmt.field =:= id,
		       Layout#layout.style =:= collapsed ->
	    Layout1 = Layout#layout { style = normal },
	    update_layout(Layout, Layout1, State);

	{I, _Fmt} when D#d.shift ->  %% add to selection
	    FID = Layout#layout.id,
	    Selected = lists:usort([{FID,I}|D#d.selected]),
	    D1 = D#d { selected = Selected },
	    State1 =  {S,D1},
	    redraw(FID, State1);

	{I, _Fmt} ->
	    FID = Layout#layout.id,
	    deselect(State, [{FID,I}])
    end.

%%
%% Commands on Selected elements:
%%   X              Hexadecimal format
%%   D              Decimal format
%%   B              Binary format
%%   O              Octal format
%%   Q              Quit application
%%   --
%%   G              Group selected bits
%%   Shift+G        Ungroup selected bits
%%   1-8            Split in groups of 1 to 8 bits
%%   T              Swap groups
%%   Ctrl+S         Save information
%%
%% Global commands
%%
command($x, Selected, State) ->
    set_base(Selected, 16, State);
command($d, Selected, State) ->
    set_base(Selected, 10, State);
command($o, Selected, State) ->
    set_base(Selected, 8, State);
command($b, Selected, State) ->
    set_base(Selected, 2, State);
command($G, Selected, State={_S,D}) when D#d.shift ->
    Fs = lists:usort([FID || {FID,_} <- Selected]),
    foldl(fun(FID,Si) -> split_half_fid(FID,Selected,Si) end, State, Fs);

command($g, Selected, _State = {S,D}) ->
    Fs = lists:usort([FID || {FID,_} <- Selected]),
    %% select min pos foreach FID and keep as selected
    Selected1 = lists:foldl(
		  fun(FID, Acc) ->
			  MinPos = lists:min([Pos || {F,Pos} <- Selected, 
						     F =:= FID]),
			  [{FID,MinPos}|Acc]
		  end, [], Fs),
    State1 = {S, D#d { selected = Selected1 }},
    foldl(fun(FID,Si) -> merge_fid(FID, Selected, Si) end, State1, Fs);

command($s, Selected, State={_S,D}) when D#d.ctrl ->
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
		  %% io:format("SAVE FID=0x~s Bits=~w\n", [integer_to_list(FID,16), Bs]),
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
    Dir = case os:getenv("HOME") of
    	       false -> "/tmp";
	       Home -> Home
	  end,
    file:write_file(filename:join(Dir, "candy.txt"), 
		    [Bytes,
		     " This line and the following lines are comments\n"]),
    State;
command($q, _Selected, State) ->
    erlang:halt(0),
    State;
command(I, Selected, State) when I >= $1, I =< $8 ->
    Fs = lists:usort([FID || {FID,_} <- Selected]),
    foldl(fun(FID,Si) -> split_fid(FID, Selected, I-$0, Si) end, State, Fs);

command(up, _Selected, State) ->
    scroll_up(State);
command(down, _Selected, State) ->
    scroll_down(State);

command(_Symbol, _Selected, State) ->
    %% io:format("command ~p selected=~p\n", [_Symbol, _Selected]),
    State.

%% deselect all and selecte the NewSelected
deselect({S,D}, NewSelected) ->
    Fs = lists:usort([FID || {FID,_} <- D#d.selected ++ NewSelected]),
    State = {S, D#d { selected = NewSelected }},
    foldl(fun(FID,Si) -> redraw(FID, Si) end, State, Fs).

scroll_up(_State={S,D}) ->
    Y = max(0, D#d.view_ypos - ?YSTEP),
    State1 = {S, D#d { view_ypos = Y }},
    redraw(State1).

scroll_down(_State={S,D}) ->
    H = if D#d.hscroll =:= undefined -> S#s.height;
	   true -> S#s.height - ?SCROLL_BAR_SIZE
	end,
    B = max(0, (D#d.view_bottom+S#s.bottom_offset) - H),
    Y = min(D#d.view_ypos + ?YSTEP, B),
    State1 = {S, D#d { view_ypos = Y }},
    redraw(State1).

scroll_left(_State={S,D}) ->
    X = max(0, D#d.view_xpos -  ?XSTEP),
    State1 = {S, D#d { view_xpos = X }},
    redraw(State1).

scroll_right(_State={S,D}) ->
    W = if D#d.vscroll =:= undefined -> S#s.width;
	   true ->  S#s.width - ?SCROLL_BAR_SIZE
	end,
    R = max(0, (D#d.view_right - S#s.right_offset) - W),
    X = min(D#d.view_xpos + ?XSTEP, R),
    State1 = {S, D#d { view_xpos = X }},
    redraw(State1).

%% Set base to 'Base' in selected cells
set_base(Selected, Base, State) ->
    foldl(
      fun({FID,I}, Si) ->
	      L= #layout{format=Format} = lookup_layout(FID,Si),
	      Fmt = element(I, Format),
	      if Fmt#fmt.field =:= data ->
		      Fmt1 = Fmt#fmt { base = Base },
		      Format1 = setelement(I, Format, Fmt1),
		      L1 = L#layout{format=Format1},
		      Si1 = insert_layout(L1, Si),
		      update_layout(L, L1, Si1);
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
    State1 = insert_layout(L1, State),
    update_layout(L, L1, State1).

split_half_fid(FID, Selected, State) ->
    Sel = lists:filter(fun({F,_}) -> F =:= FID end, Selected),
    PosList = lists:sort([Pos || {_,Pos} <- Sel]),
    L = lookup_layout(FID, State),
    Format = L#layout.format,
    FmtList1 = split_halfs(1, tuple_to_list(Format), [], PosList),
    Format1 = list_to_tuple(FmtList1),
    L1 = L#layout { format=Format1 },
    State1 = insert_layout(L1, State),
    update_layout(L, L1, State1).

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
			%% io:format("merged ~p and ~p to ~p\n",
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
		    io:format("fixme: split bits ~w\n", [Fmt#fmt.bits]),
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
		    io:format("fixme: split bits ~w\n", [Fmt#fmt.bits]),
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
     #fmt { field=frequency,base=10,type={float,4,1},bits=[]},
     #fmt { field=id,base=0,type={enum,{"-","X"}},bits=[{0,1}]},
     #fmt { field=id,base=0,type={enum,{"-","R"}},bits=[{1,1}]},
     #fmt { field=id,base=0,type={enum,{"-","E"}},bits=[{2,1}]},
     #fmt { field=len,base=16,type=unsigned,bits=[{0,4}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{0,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{8,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{16,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{24,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{32,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{40,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{48,8}]},
     #fmt { field=data, base=16, type=unsigned, bits=[{56,8}]}
    }.

bits_int8(Pos)  -> [{Pos,8}].
bits_int16(Pos) -> [{Pos,16}].
bits_int32(Pos) -> [{Pos,32}].

bits_int16_LE(Pos) -> [{Pos+8,8},{Pos,8}].
bits_int32_LE(Pos) -> [{Pos+24,8},{Pos+16},{Pos+8},{Pos,8}].

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

redraw_anim(State={S,_}) ->
    case ets:first(S#s.frame_anim) of
	'$end_of_table' -> 
	    false;
	First ->
	    {Remove,Update} = redraw_anim_(First, [], [], State),
	    ets:delete(S#s.frame_anim, Remove),
	    lists:foreach(
	      fun(Key) ->
		      %% fixme: use update couter
		      case ets:lookup(S#s.frame_anim, Key) of
			  [] -> ok;
			  [{_,Val}] ->
			      Val1 = max(0, Val-10),
			      ets:insert(S#s.frame_anim, {Key,Val1})
		      end
	      end, Update),
	    Update =/= []
    end.

redraw_anim_(Key={FID,Pos}, Remove, Update, State={S,_}) ->
    [Frame] = ets:lookup(S#s.frame, FID),
    Next = ets:next(S#s.frame_anim, Key),
    case redraw_pos(FID,Pos,Frame,State) of
	true -> %% remove
	    redraw_anim_(Next, [Key|Remove], Update, State);
	false ->
	    redraw_anim_(Next, Remove, [Key|Update], State)
    end;
redraw_anim_('$end_of_table', Remove, Update, _State) ->
    {Remove, Update}.

%% no need to clip since we always draw scollbar if needed
redraw(State={S,_D}) ->
    HBar = horizontal_scrollbar(State),
    VBar = vertical_scrollbar(State),
    epx:pixmap_fill(S#s.pixels, ?BACKGROUND_COLOR),
    State1 = redraw_all(State),
    State2 = draw_scrollbar(State1,?SCROLL_VERTICAL,HBar),
    State3 = draw_scrollbar(State2,?SCROLL_HORIZONTAL,VBar),
    update_window(State3).

%% decfine clip rect for content drawing depending on scrollbar information
clip_rect(_State={S,_D}, none, none) ->
    {0,0,S#s.width,S#s.height};
clip_rect(_State={S,_D}, none, right) ->
    {0,0,S#s.width-?SCROLL_BAR_SIZE,S#s.height};
clip_rect(_State={S,_D}, none, left) ->
    {?SCROLL_BAR_SIZE,0,S#s.width-?SCROLL_BAR_SIZE,S#s.height};
clip_rect(_State={S,_D}, bottom, none) ->
    {0,0,S#s.width,S#s.height-?SCROLL_BAR_SIZE};
clip_rect(_State={S,_D}, bottom, right) ->
    {0,0,S#s.width-?SCROLL_BAR_SIZE,S#s.height-?SCROLL_BAR_SIZE};
clip_rect(_State={S,_D}, bottom, left) ->
    {?SCROLL_BAR_SIZE,0,S#s.width-?SCROLL_BAR_SIZE,S#s.height-?SCROLL_BAR_SIZE};
clip_rect(_State={S,_D}, top, none) ->
    {0,?SCROLL_BAR_SIZE,S#s.width,S#s.height-?SCROLL_BAR_SIZE};
clip_rect(_State={S,_D}, top, right) ->
    {0,?SCROLL_BAR_SIZE,S#s.width-?SCROLL_BAR_SIZE,S#s.height-?SCROLL_BAR_SIZE};
clip_rect(_State={S,_D}, top, left) ->
    {?SCROLL_BAR_SIZE,?SCROLL_BAR_SIZE,
     S#s.width-?SCROLL_BAR_SIZE,S#s.height-?SCROLL_BAR_SIZE}.

%% set clip to window content (exclude scrollbar area)
%% return Old clip rectangle
clip_window_content(State={S,_D}) ->
    SaveClip = epx:pixmap_info(S#s.pixels, clip),
    HBar = horizontal_scrollbar(State),
    VBar = vertical_scrollbar(State),
    ClipRect = clip_rect(State, HBar, VBar),
    epx:pixmap_set_clip(S#s.pixels, ClipRect),
    SaveClip.

%% use for restore saved clip rect
set_clip_rect(_State={S,_D}, ClipRect) ->
    epx:pixmap_set_clip(S#s.pixels, ClipRect).

%% redraw all Layouts 
redraw_all(State) ->
    each_layout(fun(Layout, State1) ->
			redraw_layout_(Layout, State1)
		end, State).

redraw_(FID, State) ->
    Layout = lookup_layout(FID, State),
    redraw_layout_(Layout, State).

redraw(FID, State={_S,_D}) ->
    SaveClip = clip_window_content(State),
    Layout = lookup_layout(FID, State),
    State1 = redraw_layout_(Layout, State),
    set_clip_rect(State1, SaveClip),
    repaint_layout(Layout, State1, 0).

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

fold_layout_(Fun, Acc, _Tab, '$end_of_table', _State) ->
    Acc;
fold_layout_(Fun, Acc, Tab, FID, State) ->
    Layout = lookup_layout(FID, State),
    Acc1 = Fun(Layout, Acc, State),
    fold_layout_(Fun, Acc1, Tab, ets:next(Tab, FID), State).
    
redraw_layout_(Layout, State={S,_D}) ->
    #layout{ id=FID, color=Color,format=Format} = Layout,
    %% Count = ets:lookup_element(S#s.frame_counter, FID, 2),
    draw_layout_background(Layout, State),
    [Frame] = ets:lookup(S#s.frame, FID),
    case Layout#layout.style of
	deleted ->
	    State;
	collapsed ->
	    _Remove = redraw_fmt(FID,?ID_FMT_POSITION,Color,
				 element(?ID_FMT_POSITION,Format),Frame,State),
	    State;
	_ ->
	    redraw_frame(FID,1,Color,Format,Frame,State)
    end.

redraw_frame(FID,Pos,Color,Format,Frame,State) when Pos =< tuple_size(Format) ->
    Fmt = element(Pos, Format),
    _Remove = redraw_fmt(FID,Pos,Color,Fmt,Frame,State),
    redraw_frame(FID,Pos+1,Color,Format,Frame,State);
redraw_frame(_FID,_Pos,_Color,_Format,_Frame,State) ->
    State.

redraw_pos(FID,Pos,Frame,State) ->
    #layout{ color=Color, format=Format} = Layout = lookup_layout(FID, State),
    case Layout#layout.style of
	collapsed ->
	    redraw_pos(FID,?ID_FMT_POSITION,Color,Format,Frame,State);
	deleted ->
	    true;
	_ ->
	    redraw_pos(FID,Pos,Color,Format,Frame,State)
    end.

redraw_pos(FID,Pos,Color,Format,Frame,State) ->
    if Pos > tuple_size(Format) ->
	    true;
       true ->
	    Fmt = element(Pos,Format),
	    redraw_fmt(FID,Pos,Color,Fmt,Frame,State)
    end.

redraw_fmt(FID,Pos,Color,Fmt,Frame,State={S,D}) ->
    #fmt {x=X0,y=Y0,width=W,height=H} = Fmt,
    X = X0 - D#d.view_xpos,
    Y = Y0 - D#d.view_ypos,
    {Remove,TextColor} = highlight(FID,Pos,Color,{X,Y,W,H},State),
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
	    epx:draw_rectangle(S#s.pixels, {X,Y,W,H}),
	    epx_gc:set_border_width(0);
       true ->
	    ok
    end,

    %% draw base indicator, only for data fields
    if Fmt#fmt.field =:= data ->
	    case Fmt#fmt.base of
		2  ->
		    epx:pixmap_put_pixels(S#s.pixels,
					  X+1,Y+1,6,7,argb,bin_icon(),blend),
		    epx:draw_rectangle(S#s.pixels,{X,Y,8,9});
		8  ->
		    epx:pixmap_put_pixels(S#s.pixels,
					  X+1,Y+1,6,7,argb,oct_icon(),blend),
		    epx:draw_rectangle(S#s.pixels,{X,Y,8,9});
		16 ->
		    epx:pixmap_put_pixels(S#s.pixels,
					  X+1,Y+1,6,7,argb,hex_icon(),blend),
		    epx:draw_rectangle(S#s.pixels,{X,Y,8,9});
		10 ->
		    epx:pixmap_put_pixels(S#s.pixels,
					  X+1,Y+1,6,7,argb,dec_icon(),blend),
		    epx:draw_rectangle(S#s.pixels,{X,Y,8,9});
		0  ->
		    false
	    end;
       true ->
	    false
    end,
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
			String1 = io_lib:format(Ffmt, [(1000*N)/Td]),
			ets:insert(S#s.frame_freq,
				   {FID,Time1,Count1,String1}),
			String1;
		   true ->
			String0
		end,
	    Ya = Y+1+S#s.glyph_ascent,
	    Offs = 2,
	    epx:draw_string(S#s.pixels,X+Offs,Ya,String);
	hide ->
	    epx:pixmap_copy_area(S#s.hide, 
				 S#s.pixels,
				 0, 0, X+2, Y+2, 16, 16, [blend]);
	_ ->
	    String = fmt_bits(Fmt#fmt.type, Fmt#fmt.base, BitsData),
	    Ya = Y+1+S#s.glyph_ascent,
	    Offs = if Fmt#fmt.base > 0, Fmt#fmt.field =:= data -> 6+2;
		      true -> 2
		   end,
	    epx:draw_string(S#s.pixels, X+Offs, Ya, String)
    end,
    Remove.

%% "delete" the layout by drawing layout background color
draw_layout_background(Layout, State) ->
    Color = Layout#layout.color,
    draw_layout_rectangle(Layout, Color#color.background, State,
			  Layout#layout.width).

%% prepare layout by painting the layout background color
clear_layout_background(Layout, State={S,_}) ->
    draw_layout_rectangle(Layout, S#s.background_color, State, 
			  S#s.width).

draw_layout_rectangle(Layout, Color, _State={S,D}, Width) ->
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(Color),
    X = Layout#layout.x - D#d.view_xpos,
    Y = Layout#layout.y - D#d.view_ypos,
    Rect = {X,Y,Width,Layout#layout.height},
    epx:draw_rectangle(S#s.pixels, Rect).

draw_scrollbar(State, left, HBar) ->
    draw_vertical_scrollbar(State, 0, HBar);
draw_scrollbar(State={S,_D}, right, HBar) ->
    draw_vertical_scrollbar(State, S#s.width-?SCROLL_BAR_SIZE, HBar);
draw_scrollbar(State, top, VBar) ->
    draw_horizontal_scrollbar(State, 0, VBar);
draw_scrollbar(State={S,_D}, bottom, VBar) ->
    draw_horizontal_scrollbar(State, S#s.height-?SCROLL_BAR_SIZE, VBar).

vertical_scrollbar(_State={S,D}) ->
    WH = S#s.height,
    VH = D#d.view_bottom - D#d.view_top,
    if VH > WH ->  ?SCROLL_VERTICAL;
       true  -> none
    end.
	    
draw_vertical_scrollbar(_State={S,D}, X0, HBar) ->
    WH = if HBar =:= none -> S#s.height;
	    true -> S#s.height - ?SCROLL_BAR_SIZE
	 end,
    VH = D#d.view_bottom - D#d.view_top,
    if VH > WH ->
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_fill_color(?SCROLL_BAR_COLOR),
	    Y0 = case HBar of
		     none   -> 0;
		     bottom -> 0;
		     top    -> ?SCROLL_BAR_SIZE
		 end,
	    Rect = {X0,Y0,?SCROLL_BAR_SIZE,WH},
	    DrawRect = {X0,0,?SCROLL_BAR_SIZE,S#s.height},
	    epx:draw_rectangle(S#s.pixels, DrawRect),
	    epx_gc:set_fill_color(?SCROLL_HNDL_COLOR),
	    Part = WH / VH,
	    HandleLength = trunc(Part*WH),
	    Top = D#d.view_ypos,
	    HandlePos = trunc(Part*Top),
	    Pad = (?SCROLL_BAR_SIZE - ?SCROLL_HNDL_SIZE) div 2,
	    HRect = {X0+Pad,Y0+HandlePos,?SCROLL_HNDL_SIZE,HandleLength},
	    epx:draw_roundrect(S#s.pixels,HRect,5,5),
	    {S,D#d { vscroll=Rect, vhndl=HRect }};
       true ->
	    {S,D#d { vscroll=undefined, vhndl=undefined }}
    end.

horizontal_scrollbar(_State={S,D}) ->
    WW = S#s.width,
    VW = D#d.view_right - D#d.view_left,
    case VW > WW of
	true -> ?SCROLL_HORIZONTAL;
	false -> none
    end.
	    
%% fixme remove vertical scrollbar if present!
draw_horizontal_scrollbar({S,D}, Y0, VBar) ->
    WW = if VBar =:= none -> S#s.width;
	    true -> S#s.width - ?SCROLL_BAR_SIZE
	 end,
    VW = D#d.view_right - D#d.view_left,
    if VW > WW ->
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_fill_color(?SCROLL_BAR_COLOR),
	    X0 = case VBar of
		     none -> 0;
		     left -> 0;
		     right -> ?SCROLL_BAR_SIZE
		 end,
	    Rect = {X0,Y0,WW,?SCROLL_BAR_SIZE},
	    DrawRect = {0,Y0,S#s.width,?SCROLL_BAR_SIZE},
	    epx:draw_rectangle(S#s.pixels, DrawRect),
	    epx_gc:set_fill_color(?SCROLL_HNDL_COLOR),
	    Part = WW / VW,
	    Left = D#d.view_xpos,
	    HandleLength = trunc(Part*WW),
	    HandlePos = trunc(Part*Left),
	    Pad = (?SCROLL_BAR_SIZE - ?SCROLL_HNDL_SIZE) div 2,
	    HRect = {HandlePos,Y0+Pad,HandleLength,?SCROLL_HNDL_SIZE},
	    epx:draw_roundrect(S#s.pixels,HRect,5,5),
	    {S,D#d { hscroll=Rect, hhndl=HRect }};
       true ->
	    {S,D#d { hscroll=undefined, hhndl=undefined }}
    end.		
    

%% draw hightlight background return text color and flag to signal remove
highlight(FID, Pos, Color, Rect, _State={S,_}) ->
    case ets:lookup(S#s.frame_anim, {FID,Pos}) of
	[] ->
	    {true, Color#color.foreground};
	[{_,0}] ->
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_fill_color(Color#color.background),
	    epx:draw_rectangle(S#s.pixels,Rect),
	    {true,  Color#color.foreground};
	[{_,Val}] ->
	    B1 =  Color#color.background1,
	    B0 =  Color#color.background,
	    B  = blend(Val, B1, B0),
	    epx_gc:set_fill_style(solid),
	    epx_gc:set_fill_color(B),
	    epx:draw_rectangle(S#s.pixels,Rect),
	    T1 =  Color#color.foreground1,
	    T0 =  Color#color.foreground,
	    T  = blend(Val, T1, T0),
	    {false, T}
    end.

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


get_bits(#fmt { type={string,NameBits} }, _Frame) ->
    NameBits;
get_bits(#fmt { field=frequency }, _Frame) -> 
    <<>>;
get_bits(#fmt { field=hide }, _Frame) -> 
    <<>>;
get_bits(Fmt, Frame) ->
    Data = case Fmt#fmt.field of
	       data -> extend_bits(Frame#can_frame.data, 64);
	       id   -> <<(Frame#can_frame.id):32>>;
	       len  -> <<(Frame#can_frame.len):4>>
	   end,
    collect_bits(Fmt#fmt.bits, Data).

extend_bits(Data, MinSize) when bit_size(Data) >= MinSize ->
    Data;
extend_bits(Data, MinSize) ->
    Size = bit_size(Data),
    Pad = MinSize - Size,
    <<Data/bitstring, 0:Pad>>.

repaint_layout(Layout, State) ->
    repaint_layout(Layout, State, 0).
repaint_layout(Layout, State={_S,D}, MinW) ->
    #layout { x=X0, y=Y0, width=W0, height=H0 } = Layout,
    W1 = max(W0,MinW),
    H1 = H0,
    X1 = X0 - D#d.view_xpos,
    Y1 = Y0 - D#d.view_ypos,
    update_window(State, {X1,Y1,W1,H1}).
    
update_window(State={S,_D},_R={X,Y,W,H}) ->
    epx:pixmap_draw(S#s.pixels, S#s.window, X, Y, X, Y, W, H),
    epx:sync(S#s.window),
    State.

update_window(State={S,_D}) ->
    update_window(State, {0,0,S#s.width,S#s.height}).

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

position_normal(Layout, State={S,_D}) ->
    I = Layout#layout.pos-1,
    Y = S#s.top_offset + I*(S#s.row_height+S#s.row_pad),
    X = S#s.left_offset,
    {X1,_Y1,Format} = 
	position_format(X, Y, 1, Layout#layout.format, [], State),
    Width = (X1-X-2)+1,
    Height = S#s.row_height,
    Layout#layout { x=X,y=Y,width=Width,height=Height,format=Format }.    

position_collapsed(Layout, _State={S,_D}) ->
    J = Layout#layout.pos-1,
    Fmt = element(?ID_FMT_POSITION, Layout#layout.format), %% FIXME!
    Row = 1,  %% calculate row from bottom left to right
    Height = S#s.row_height,
    Num    = num_glyphs(Fmt),
    Wide   = 8*S#s.glyph_width + 4,
    Width  = Num*S#s.glyph_width + 4,
    X = S#s.left_offset + J*Wide,
    Y = S#s.height - Row*Height - S#s.bottom_offset,
    Fmt1 = Fmt#fmt { x=X, y=Y, width=Width, height=Height },
    Format = setelement(?ID_FMT_POSITION, Layout#layout.format, Fmt1),
    Layout#layout { x=X,y=Y,width=Wide+2,height=Height,format=Format}.

position_format(X,Y,I,Format,Acc,State={S,_D}) when I =< tuple_size(Format) ->
    Fmt = element(I, Format),
    Num = num_glyphs(Fmt),
    Width0 = Num*S#s.glyph_width,
    Wind = if Fmt#fmt.field =:= data -> 6; %% what was this?
	      true -> 0
	   end,
    Width = Wind + Width0 + 4,
    Height = S#s.row_height,
    Fmt1 = Fmt#fmt { x=X, y=Y, width=Width, height=Height },
    position_format(X+Width+2, Y, I+1, Format, [Fmt1|Acc], State);
position_format(X, Y, _I, _Format, Acc, _State) ->
    {X, Y, list_to_tuple(lists:reverse(Acc)) }.

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
    pow(10, N).

number_of_digits(2,  Size) -> Size;
number_of_digits(8,  Size) -> ((Size+2) div 3);
number_of_digits(16, Size) -> ((Size+3) div 4);
number_of_digits(10, Size) -> trunc(math:log10((1 bsl Size)-1)) + 1.

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
    <<_:P, Bits:L/bitstring, _/bitstring>> = Data,
    collect_bits_(Ps, Data, Bits).

collect_bits_([{P,L}|Ps], Data, Acc) ->
    <<_:P, Bits:L/bitstring, _/bitstring>> = Data,
    collect_bits_(Ps, Data, <<Acc/bitstring, Bits/bitstring>>);
collect_bits_([], _Data, Acc) ->
    Acc.

lookup_layout(FID, _State={S,_D}) ->
    [L] = ets:lookup(S#s.frame_layout, FID),  %% lookup element
    L.

insert_layout(Layout, _State={S,D}) ->
    ets:insert(S#s.frame_layout, Layout),
    L = min(Layout#layout.x, D#d.view_left),
    R = max(Layout#layout.x + Layout#layout.width, D#d.view_right),
    T = min(Layout#layout.y, D#d.view_top),
    B = max(Layout#layout.y + Layout#layout.height, D#d.view_bottom),
    {S, D#d { view_left=L, view_right=R, view_top=T, view_bottom=B }}.

update_layout(OldLayout, NewLayout, State={S,D}) ->
    SaveClip = clip_window_content(State),

    clear_layout_background(OldLayout, State),
    Layout = position_layout(NewLayout, State),
    State1 = insert_layout(Layout, State),
    State2 = redraw_layout_(Layout, State1),

    set_clip_rect(State2, SaveClip),

    MinW = if D#d.vscroll =:= undefined ->
		   S#s.width;
	      true ->
		   S#s.width - ?SCROLL_BAR_SIZE
	   end,
    if OldLayout#layout.x =/= Layout#layout.x;
       OldLayout#layout.y =/= Layout#layout.y;
       OldLayout#layout.width =/= Layout#layout.width;
       OldLayout#layout.height =/= Layout#layout.height ->
	    %% if layout changed repaint old area
	    repaint_layout(OldLayout, State2, MinW);
       true ->
	    ok
    end,
    repaint_layout(Layout, State2, MinW).

layout_from_position(Pos, _State={S,_D}) ->
    Tab = S#s.frame_layout,
    Key = ets:first(Tab),
    layout_from_position_(Pos, Tab, Key).

layout_from_position_(_Pos, _Tab, '$end_of_table') ->
    false;
layout_from_position_(Pos={X,Y}, Tab, FID) ->
    case ets:lookup(Tab, FID) of
	[] ->
	    layout_from_position_(Pos, Tab, ets:next(Tab, FID));
	[Layout=#layout{x=X1,y=Y1,width=W,height=H}] ->
	    if X >= X1, Y >= Y1, X < X1+W, Y < Y1+H ->
		    Layout;
	       true ->
		    layout_from_position_(Pos, Tab, ets:next(Tab, FID))
	    end
    end.

fmt_from_position(Pos, Layout) ->
    fmt_from_position_(Pos, 1, Layout#layout.format).

fmt_from_position_(Pos={X,Y}, I, Format) when I =< tuple_size(Format) ->
    F = #fmt{x=X1,y=Y1,width=W,height=H} = element(I, Format),
    if X >= X1, Y >= Y1, X < X1+W, Y < Y1+H ->    
	    {I, F};
       true ->
	    fmt_from_position_(Pos, I+1, Format)
    end;
fmt_from_position_(_Pos, _I, _Format) ->
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

flush_wheel(Window) ->
    receive
	{epx_event,Window,{_,[wheel_down],_}} ->
	    flush_wheel(Window);
	{epx_event,Window,{_,[wheel_left],_}} ->
	    flush_wheel(Window);
	{epx_event,Window,{_,[wheel_right],_}} ->
	    flush_wheel(Window);
	{epx_event,Window,{_,[wheel_up],_}} ->
	    flush_wheel(Window)
    after 0 ->
	    ok
    end.

flush_configure(Win, Rect) ->
    receive
	{epx_event, Win, {configure, Rect1}} ->
	    flush_configure(Win, Rect1)
    after 0 ->
	    Rect
    end.

flush_expose(Win, Rect) ->
    receive
	{epx_event, Win, {expose, Rect1}} ->
	    flush_expose(Win, Rect1)
    after 0 ->
	    Rect
    end.

flush_motion(Win) ->
    receive
	{epx_event, Win, {motion, _Mod, _Pos}} ->
	    flush_motion(Win)
    after 0 ->
	    ok
    end.

resize_pixmap(undefined, W, H) ->
    Pixmap = next_pixmap(W,H),
    epx:pixmap_attach(Pixmap),
    Pixmap;
resize_pixmap(Pixmap, W, H) ->
    case epx:pixmap_info(Pixmap,[width,height]) of
	[{width,PW},{height,PH}] when PW < W; PH < H ->
	    epx:pixmap_detach(Pixmap),
	    Pixmap1 = next_pixmap(W,H),
	    epx:pixmap_attach(Pixmap1),
	    Pixmap1;
	_ ->
	    Pixmap
    end.

next_pixmap(W,H) ->
    NPW = 1 bsl ceil(math:log2(W)),
    NPH = 1 bsl ceil(math:log2(H)),
    epx:pixmap_create(NPW, NPH, argb).
