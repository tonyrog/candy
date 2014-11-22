%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
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

-record(layout,
	{
	  id,    %% frame id
	  x,     %% start x
	  y      %% start y
	}).

-record(state,
	{
	  width  :: integer(),
	  height :: integer(),
	  nrows = 20  :: integer(),  %% number or rows shown
	  window :: epx:epx_window(),  %% attached window
	  font   :: epx:epx_font(),
	  foreground_pixels :: epx:epx_pixmap(),
	  background_pixels :: epx:epx_pixmap(),
	  frame,          %% ets: ID -> #can_frame{}
	  frame_counter,  %% ets: ID -> Counter
	  frame_layout,   %% ets: ID -> #layout{}
	  top_offset    = 5,
	  left_offset   = 5,
	  right_offset  = 5,
	  bottom_offset = 5,
	  row_width     = 0,
	  row_height    = 0,
	  row_pad = 2
	}).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    gen_server:start(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

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
    S0 = #state{},
    {W,H}  = epx_font:dimension(Font,"0"),
    Height = S0#state.top_offset+S0#state.nrows*(H+S0#state.row_pad)
	- S0#state.row_pad + S0#state.bottom_offset,
    %% FORMAT= ID X|R L 01234567 01234567
    %% maximum width = 
    %%   ID: 3FF (11-bit) | 1FFFFFFF (29-bit)
    %% Data: 64 bit : 8*8 (16 char) | 64*1 (64 char) | X|R
    Width = (8+2+1+64)*W,
    Window = epx:window_create(40, 40, Width, Height,
			       [button_press,button_release]),
    Fg = epx:pixmap_create(Width, Height, Format),
    epx:pixmap_fill(Fg, white),
    Bg = epx:pixmap_create(Width, Height, Format),
    epx:pixmap_fill(Bg, white),
    epx:window_attach(Window),
    epx:pixmap_attach(Bg),
    
    {ok, S0#state{
	   width = Width,
	   height = Height,
	   window = Window,
	   font   = Font,
	   foreground_pixels = Fg,
	   background_pixels = Bg,
	   frame = ets:new(frame, []),
	   frame_counter = ets:new(frame_counter, []),
	   frame_layout  = ets:new(frame_layout, [{keypos,#layout.id}]),
	   row_width = Width,
	   row_height = H
	  }}.

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
    %% expand or collapse groups 
    {noreply, State};
handle_info(Frame=#can_frame{id=FID}, State) ->
    case ets:lookup(State#state.frame, FID) of
	[{_FID,Frame}] -> %% no change
	    ok;
	[{_FID,_Frame0}] ->
	    %% fixme: create a new change map for animation
	    ets:insert(State#state.frame, {FID, Frame}),
	    Yi = ets:info(State#state.frame, size),
	    ets:update_counter(State#state.frame_counter, FID, 1),
	    Layout = #layout { x = State#state.left_offset,
			       y = State#state.top_offset + 
				   (Yi-1)*(State#state.row_height+
					       State#state.row_pad) },
	    ets:insert(State#state.frame_layout, {FID, Layout});
	false ->
	    %% fixme: flash the row?
	    ets:insert(State#state.frame, {FID, Frame})
    end,
    ets:insert(State#state.frame_counter, {FID, 1}),
    refresh(FID, State),
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

refresh(FID, State) ->
    [#layout{x=X,y=Y}] = ets:lookup(State#state.frame_layout, FID),
    [Frame] = ets:lookup(State#state.frame, FID),
    Rtr = if (?CAN_RTR_FLAG band FID) =/= 0 -> $R; true -> $- end,
    Ext = if (?CAN_EFF_FLAG band FID) =/= 0 -> $X; true -> $- end,
    Err = if (?CAN_ERR_FLAG band FID) =/= 0 -> $E; true -> $- end,
    ID = FID band ?CAN_EFF_MASK,
    String0 =
	if (?CAN_EFF_FLAG band FID) =/= 0 ->
		io_lib:format("~8.16.0B:~c~c~c:~s", 
			      [ID,Rtr,Ext,Err,fmt_data(Frame)]);
	   true ->
		io_lib:format("     ~3.16.0B:~c~c~c:~s", 
			      [ID,Rtr,Ext,Err,fmt_data(Frame)])
	end,
    String = lists:flatten(String0),
    {W,H}  = epx_font:dimension(State#state.font,String),
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(white),
    epx:draw_rectangle(State#state.foreground_pixels, 
		       {X,Y,W,H}),
    Y1 = Y + epx:font_info(State#state.font, ascent),
    epx_gc:set_foreground_color(16#00000000),  %% black
    epx:draw_string(State#state.foreground_pixels, X, Y1, String),
    update_window(State).
    
    
update_window(State) ->    
    epx:pixmap_copy_to(State#state.foreground_pixels,
		       State#state.background_pixels),
    epx:pixmap_draw(State#state.background_pixels, State#state.window,
		    0, 0, 0, 0, 
		    State#state.width, State#state.height).
    
fmt_data(Frame) ->
    fmt_data(8, Frame#can_frame.len, Frame#can_frame.data).

fmt_data(0, 0, _) -> [];
fmt_data(I, 0, _) -> lists:duplicate(I,$-);
fmt_data(I, J, <<D,Bin/binary>>) -> 
    [fmt_hex8(D) | fmt_data(I-1, J-1, Bin)].
	    
fmt_hex8(X) ->
    tl(integer_to_list(16#100 + (X band 16#ff), 16)).

