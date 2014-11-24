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
-export([start/0]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("can/include/can.hrl").

-type unsigned() :: non_neg_integer().

-compile(export_all).

-record(fmt,
	{
	  x :: non_neg_integer(),
	  y :: non_neg_integer(),
	  width :: non_neg_integer(),
	  height :: non_neg_integer(),
	  hidden = false :: boolean(),
	  type = unsigned :: unsigned | signed | {enum,tuple()},
	  field = none,
	  base = 16 :: 2 | 8 | 16 | 10,
	  signed = false :: boolean(),
	  bits = [] :: [{Pos::non_neg_integer(),Length::non_neg_integer()}]
	}).

-record(layout,
	{
	  id,      %% frame id
	  pos,     %% list position
	  x     :: non_neg_integer(),   %% x offset
	  y     :: non_neg_integer(),   %% y offset
	  width :: non_neg_integer(),   %% total width
	  height :: non_neg_integer(),  %% total height
	  format = [%% id field is dynamically inserted 
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
		   ]
	}).

-record(state,
	{
	  width  :: integer(),
	  height :: integer(),
	  nrows = 32 :: integer(),     %% number or rows shown
	  window :: epx:epx_window(),  %% attached window
	  font   :: epx:epx_font(),
	  %% foreground_pixels :: epx:epx_pixmap(),
	  background_pixels :: epx:epx_pixmap(),
	  frame,          %% ets: #can_frame{}
	  frame_layout,   %% ets: #layout{}
	  frame_counter,  %% ets: ID -> Counter
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
	  row_pad = 3
	}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    dep_start(),
    can_udp:start(),
    gen_server:start(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    dep_start(),
    can_udp:start(),
    gen_server:start_link(?MODULE, [], []).


dep_start() ->
    application:start(uart),
    application:start(can),
    application:start(epx).
   
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
init([]) ->
    can_router:attach(),
    Format = argb,
    {ok,Font} = epx_font:match([{name,"Courier New"},{size,12}]),
    epx_gc:set_font(Font),
    S0 = #state{},
    {W,H}  = epx_font:dimension(Font,"0"),
    RowHeight = H + 2,
    Height = S0#state.top_offset+S0#state.nrows*(RowHeight+S0#state.row_pad)
	- S0#state.row_pad + S0#state.bottom_offset,
    BgColor = cyan,
    %% FORMAT= ID X|R|E L 01 23 45 67 01 23 45 67
    %% maximum width = 
    %%   ID: 3FF (11-bit) | 1FFFFFFF (29-bit)
    %% Data: 64 bit : 8*8 (16 char) | 64*1 (64 char) | X|R
    %% Given 8 byte groups in base 2
    Width = S0#state.left_offset + 
	1*(8*W+4+2) + 3*(1*W+4+2) + 1*(1*W+4+2) + 8*(6+8*W+4+2) - 2 +
	S0#state.right_offset,
    Window = epx:window_create(40, 40, Width, Height,
			       [button_press,button_release]),
    %% Fg = epx:pixmap_create(Width, Height, Format),
    %% epx:pixmap_fill(Fg, BgColor),
    Bg = epx:pixmap_create(Width, Height, Format),
    epx:pixmap_fill(Bg, BgColor),
    epx:window_attach(Window),
    epx:pixmap_attach(Bg),
    S1 = S0#state{
	   width = Width,
	   height = Height,
	   window = Window,
	   font   = Font,
	   glyph_width  = W,
	   glyph_height = H,
	   glyph_ascent = epx:font_info(Font, ascent),
	   background_color = BgColor,
	   %% foreground_pixels = Fg,
	   background_pixels = Bg,
	   frame = ets:new(frame, [{keypos,#can_frame.id}]),
	   frame_counter = ets:new(frame_counter, []),
	   frame_layout  = ets:new(frame_layout, [{keypos,#layout.id}]),
	   row_width = Width,
	   row_height = RowHeight
	  },
    update_window(S1),
    {ok, S1}.

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
handle_info({epx_event, Win, close}, State) 
  when Win =:= State#state.window ->
    io:format("Got window close\n", []),
    {stop, normal, State};
handle_info({epx_event, Win, {button_press,[left],{X,Y,_}}}, State) 
  when Win =:= State#state.window ->
    %% 1- find layout and fmt and rotate bwteen bases (unsigned/signed only)
    toggle_frame_base(X,Y,State),
    {noreply, State};
handle_info({epx_event, _Win, {button_release,[left],{_X,_Y,_}}}, State) ->
    {noreply, State};

handle_info(Frame=#can_frame{id=FID}, State) ->
    case ets:lookup(State#state.frame, FID) of
	[Frame] -> %% no change
	    ok;
	[_Frame0] ->
	    %% fixme: create a new change map for animation
	    ets:insert(State#state.frame, Frame);
	[] ->
	    %% fixme: flash the row?
	    Pos = ets:info(State#state.frame, size),
	    ets:insert(State#state.frame, Frame),
	    Layout0 = #layout { id=FID, pos=Pos },
	    %% concat id field first
	    Format0 = Layout0#layout.format,
	    Format1 =
		if FID band ?CAN_EFF_FLAG =/= 0 ->
			[#fmt { field=id,base=16,type=unsigned,bits=[{3,29}]} |
			 Format0];
		   true ->
			[#fmt { field=id,base=16,type=unsigned,bits=[{21,11}]} |
			 Format0]
		end,
	    Layout1 = Layout0#layout { format = Format1},
	    Layout2 = position_layout(Layout1, State),
	    ets:insert(State#state.frame_layout, Layout2),
	    ets:insert(State#state.frame_counter, {FID, 0})
    end,
    ets:update_counter(State#state.frame_counter, FID, 1),
    redraw(FID, State),
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
terminate(_Reason, State) ->
    epx:pixmap_detach(State#state.background_pixels),
    epx:window_detach(State#state.window),
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

redraw(FID, State) ->
    redraw(FID, State, 0).

redraw(FID, State, MinW) ->
    [#layout{ x=X,y=Y,width=W,height=H,format=Format}] =
	ets:lookup(State#state.frame_layout, FID),
    [Frame] = ets:lookup(State#state.frame, FID),
    %% Count = ets:lookup_element(State#state.frame_counter, FID, 2),
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(white),
    epx:draw_rectangle(State#state.background_pixels, {X,Y,W,H}),
    redraw_frame(Format, Frame, State),
    update_window(State, {X,Y,max(W,MinW),H}).

redraw_frame([Fmt=#fmt {x=X,y=Y,width=W,height=H,bits=Bits}|Fs],Frame,State) ->
    Data = case Fmt#fmt.field of
	       data -> extend_bits(Frame#can_frame.data, 64);
	       id   -> <<(Frame#can_frame.id):32>>;
	       len  -> <<(Frame#can_frame.len):4>>
	   end,
    BitsData = collect_bits(Bits, Data),
    epx_gc:set_fill_style(none),
    epx_gc:set_foreground_color(black),
    epx:draw_rectangle(State#state.background_pixels, {X,Y,W,H}),
    %% draw base indicator, only for data fields
    if Fmt#fmt.field =:= data ->
	    case Fmt#fmt.base of
		2  -> 
		    epx:pixmap_put_pixels(State#state.background_pixels,
					  X+1,Y+1,6,7,argb,bin_icon(),blend),
		    epx:draw_rectangle(State#state.background_pixels,{X,Y,8,9});
		8  ->
		    epx:pixmap_put_pixels(State#state.background_pixels,
					  X+1,Y+1,6,7,argb,oct_icon(),blend),
		    epx:draw_rectangle(State#state.background_pixels,{X,Y,8,9});
		16 ->
		    epx:pixmap_put_pixels(State#state.background_pixels,
					  X+1,Y+1,6,7,argb,hex_icon(),blend),
		    epx:draw_rectangle(State#state.background_pixels,{X,Y,8,9});
		10 ->
		    epx:pixmap_put_pixels(State#state.background_pixels,
					  X+1,Y+1,6,7,argb,dec_icon(),blend),
		    epx:draw_rectangle(State#state.background_pixels,{X,Y,8,9});
		0  -> 
		    false
	    end;
       true ->
	    false
    end,
    epx_gc:set_foreground_color(16#00000000), %% black
    String = fmt_bits(Fmt#fmt.type,Fmt#fmt.base, BitsData),
    Ya = Y+1+State#state.glyph_ascent,
    Offs = if Fmt#fmt.base > 0, Fmt#fmt.field =:= data -> 6+2;
	      true -> 2
	   end,
    epx:draw_string(State#state.background_pixels, X+Offs, Ya, String),
    redraw_frame(Fs,Frame,State);
redraw_frame([],_Frame,_State) ->
    ok.

extend_bits(Data, MinSize) when bit_size(Data) >= MinSize ->
    Data;
extend_bits(Data, MinSize) ->
    Size = bit_size(Data),
    Pad = MinSize - Size,
    <<Data/bitstring, 0:Pad>>.
    
update_window(State) ->
    update_window(State, {0,0,State#state.width,State#state.height}).
    
update_window(State,{X,Y,W,H}) ->
%%    epx:pixmap_copy_to(State#state.foreground_pixels,
%%		       State#state.background_pixels),
    epx:pixmap_draw(State#state.background_pixels, State#state.window,
		    X, Y, X, Y,
		    W, H).

%% recalulate Layout
position_layout(Layout, State) ->
    Y = State#state.top_offset + 
	Layout#layout.pos*(State#state.row_height+State#state.row_pad),
    X = State#state.left_offset,
    {X1,_Y1,Format} = position_format(X, Y, Layout#layout.format, [], State),
    Width = (X1-X-2)+1,
    Height = State#state.row_height,
    Layout#layout { x=X, y=Y, width=Width, height=Height,
		    format=Format }.

position_format(X, Y, [Fmt|Fs], Acc, State) ->
    Width0 = case Fmt#fmt.type of
		 unsigned ->
		     Size = lists:sum([Len || {_Pos,Len} <- Fmt#fmt.bits]),
		     N = number_of_digits(Fmt#fmt.base, Size),
		     N*State#state.glyph_width;
		 signed ->
		     Size = lists:sum([Len || {_Pos,Len} <- Fmt#fmt.bits]),
		     N = number_of_digits(Fmt#fmt.base, Size),
		     %% one extra char for -/+ sign
		     (N+1)*State#state.glyph_width;
		 {enum,Names} when is_tuple(Names) ->
		     N = lists:max([length(Name)||Name <-tuple_to_list(Names)]),
		     N*State#state.glyph_width
	     end,
    Wind = if Fmt#fmt.field =:= data -> 6;
	      true -> 0
	   end,
    Width = Wind + Width0 + 4,
    Height = State#state.row_height,
    Fmt1 = Fmt#fmt { x=X, y=Y, width=Width, height=Height },
    position_format(X+Width+2, Y, Fs, [Fmt1|Acc], State);
position_format(X, Y, [], Acc, _State) ->
    {X, Y, lists:reverse(Acc)}.


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
    fmt_num(Base, Size, Number).

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

collect_bits(Bits, Data) when is_bitstring(Data) ->
    collect_bits(Bits, Data, <<>>).

collect_bits([{P,L}|Ps], Data, Acc) ->
    <<_:P, Bits:L/bitstring, _/bitstring>> = Data,
    collect_bits(Ps, Data, <<Acc/bitstring, Bits/bitstring>>);
collect_bits([], _Data, Acc) ->
    Acc.

%%
%%
%%
toggle_frame_base(X,Y,State) ->
    case layout_from_position(X, Y, State) of
	false ->
	    false;
	Layout ->
	    case toggle_fmt(X, Y, Layout) of
		false ->
		    false;
		NewLayout ->
		    %% clear old layout
		    epx_gc:set_fill_style(solid),
		    epx_gc:set_fill_color(State#state.background_color),
		    epx:draw_rectangle(State#state.background_pixels,
				       {Layout#layout.x,
					Layout#layout.y,
					Layout#layout.width,
					Layout#layout.height}),
		    Layout1 = position_layout(NewLayout, State),
		    ets:insert(State#state.frame_layout, Layout1),
		    %% draw the new updated layout
		    redraw(Layout#layout.id, State, Layout#layout.width),
		    true
	    end
    end.

toggle_fmt(X, Y, Layout) ->
    Format = Layout#layout.format,
    case toggle_fmt_(X, Y, Format) of
	Format -> false;
	NewFormat ->
	    Layout#layout { format=NewFormat}
    end.

%% change base 2->8->16->10->2
toggle_fmt_(X, Y, [Fmt=#fmt{x=X1,y=Y1,width=W,height=H}|Fs]) ->
    if X >= X1, Y >= Y1, X < X1+W, Y < Y1+H ->
	    if Fmt#fmt.field =:= data, Fmt#fmt.type =:= unsigned ->
		    NextBase = case Fmt#fmt.base of
				   0 -> 0;  %% enum | strings
				   2 -> 8;
				   8 -> 16;
				   16 -> 10;
				   10 -> 2
			       end,
		    [Fmt#fmt { base = NextBase }|Fs];
	       true ->
		    [Fmt|Fs]
	    end;
       true ->
	    [Fmt|toggle_fmt_(X, Y, Fs)]
    end;
toggle_fmt_(_X, _Y, []) ->
    [].


layout_from_position(X, Y, State) ->
    Tab = State#state.frame_layout,
    Key = ets:first(Tab),
    layout_from_position_(X, Y, Tab, Key).

layout_from_position_(_X, _Y, _Tab, '$end_of_table') ->
    false;
layout_from_position_(X, Y, Tab, FID) ->
    case ets:lookup(Tab, FID) of
	[] ->
	    layout_from_position_(X, Y, Tab, ets:next(Tab, FID));
	[Layout=#layout{x=X1,y=Y1,width=W,height=H}] ->
	    if X >= X1, Y >= Y1, X < X1+W, Y < Y1+H ->
		    Layout;
	       true ->
		    layout_from_position_(X, Y, Tab, ets:next(Tab, FID))
	    end
    end.

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

