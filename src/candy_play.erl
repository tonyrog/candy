%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    Play CANUSB log file (with filestamp)
%%% @end
%%% Created :  9 Mar 2019 by Tony Rogvall <tony@rogvall.se>

-module(candy_play).

-export([file/1, file/2]).

-include_lib("can/include/can.hrl").

-compile(export_all).

file(File) ->
    file(File,0).

file(File,Repeat) when is_integer(Repeat), Repeat >= -1 ->
    (catch error_logger:tty(false)),
    application:start(lager),
    can_udp:start(0, [{ttl,0}]),
    case file:open(File,[read,binary,raw]) of
	{ok,Fd} ->
	    try replay(Fd, Repeat, ?CAN_NO_TIMESTAMP) of
		Result -> Result
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.

replay(Fd, Repeat, LastStamp) ->
    case file:read_line(Fd) of
	eof ->
	    if Repeat =:= -1 ->
		    file:position(Fd, 0),
		    replay(Fd, -1, ?CAN_NO_TIMESTAMP);
	       Repeat > 0 ->
		    file:position(Fd, 0),
		    replay(Fd, Repeat-1, ?CAN_NO_TIMESTAMP);
	       Repeat =:= 0 ->
		    ok
	    end;
	%% T    8     1      -- Len=8 --        Tstamp (16bit)
	%% T 0240162A 8 E0 10 00 40 07 00 19 00 4337
	{ok,Line} ->
	    case decode(Line) of
		ignore ->
		    replay(Fd, Repeat, LastStamp);
		{ok,TimeHw,Frame=#can_frame { ts=Stamp }} ->
		    can:send(Frame),
		    Wait = timediff(TimeHw,Stamp, LastStamp),
		    timer:sleep(Wait),
		    replay(Fd, Repeat, Stamp);
		{error,Reason} ->
		    io:format("error: ~p, line ~p\n", [Reason,Line]),
		    timer:sleep(2000),
		    replay(Fd, Repeat, -1)
	    end;
	{ok,Line} ->
	    io:format("bad line format ~p\n", [Line]),
	    timer:sleep(2000),
	    replay(Fd, Repeat, -1)
    end.

%% return wait time in millis seconds
%% timestamps are in range 0 - 16#EA5F (59999)
timediff(_,_New, ?CAN_NO_TIMESTAMP) -> 1;
timediff(canusb,New, New) -> 1;  %% just yeild a bit
timediff(canusb,New, Old) when New > Old -> New - Old;
timediff(canusb,New, Old) -> 60000 - (Old - New);
timediff(pcan, New, Old) when New > Old -> New - Old;
timediff(pcan, _, _) -> 1;
timediff(vbox, New, Old) when New > Old -> New - Old;
timediff(vbox, _, _) ->  1.

%% Filter various comments
decode(<<"Dearborn ", _/binary>>) -> ignore;
decode(<<"--------", _/binary>>) -> ignore;
decode(<<"Created ", _/binary>>) -> ignore;
decode(<<"Timestamp ", _/binary>>) -> ignore;
decode(<<"\n">>) -> ignore;
decode(<<"Msg ID ", _/binary>>) -> ignore;
decode(<<";", _/binary>>) -> ignore;   %% PCAN comment
%% CANUSB $T
decode(<<$T,ID29:8/binary,L,Tail/binary>>) ->
    ID = binary_to_integer(ID29,16),
    Len = binary_to_integer(<<L>>, 16),
    case Tail of
	<<Message:Len/binary-unit:16, Ts:4/binary, $\n>> ->
	    Stamp = binary_to_integer(Ts, 16),
	    {ok, canusb, can:create(ID,Len,true,false,0,data(Message),Stamp)};
	<<Message:Len/binary-unit:16, $\n>> ->
	    {ok, canusb, can:create(ID,Len,true,false,0,data(Message))};
	_ ->
	    {error, bad_format}
    end;
%% CANUSB $t
decode(<<$t,ID11:3/binary,L,Tail/binary>>) ->
    ID = binary_to_integer(ID11,16),
    Len = binary_to_integer(<<L>>, 16),
    case Tail of
	<<Message:Len/binary-unit:16, Ts:4/binary, $\n>> ->
	    Stamp=binary_to_integer(Ts, 16),
	    {ok, canusb, can:create(ID,Len,false,false,0,data(Message),Stamp)};
	<<Message:Len/binary-unit:16, $\n>> ->
	    {ok, canusb, can:create(ID,Len,false,false,0,data(Message))};
	_ ->
	    {error, bad_format}
    end;
decode(Line) ->
    Parts = case re:split(Line, "[\t\s\n]+", [trim]) of
		[<<>>|LParts] -> LParts; %% skip leading blans
		Parts0 -> Parts0
	    end,
    case Parts of
	[ID0, Time, <<"CH#",_N>>, <<L>> | Rest] ->
	    Len = binary_to_integer(<<L>>, 16),
	    case collect_message(Len, Rest, <<>>) of
		{Data, [<<"RX">>]} ->
		    ID = binary_to_integer(ID0,16),
		    Ext = (ID > ?CAN_SFF_MASK),  %% must be extended
		    Stamp = decode_ts(Time),
		    {ok, vbox, can:create(ID,Len,Ext,false,0,Data,Stamp)};
		_ ->
		    {error,bad_format}
	    end;
	[_MsgNum, Time, <<"Rx">>, ID0, <<L>> | Rest] ->
	    Len = binary_to_integer(<<L>>, 16),
	    case collect_message(Len, Rest, <<>>) of
		{Data, []} ->
		    ID = binary_to_integer(ID0,16),
		    Ext = (ID > ?CAN_SFF_MASK),  %% must be extended
		    Stamp = trunc(binary_to_float(Time)),
		    {ok, pcan, can:create(ID,Len,Ext,false,0,Data,Stamp)};
		_ ->
		    {error,bad_format}
	    end;
	_ ->
	    {error, bad_format}
    end.

collect_message(0, Rest, Data) ->  {Data, Rest};
collect_message(I, [Byte|Bs], Data) -> 
    B = binary_to_integer(Byte, 16),
    collect_message(I-1, Bs, <<Data/binary, B>>).

%% convert ascii data to binary
%% 010203 .. even number of hex digits
data(Message) ->
    <<<<(binary_to_integer(<<C1,C2>>,16))>> || <<C1,C2>> <= Message>>.

%% convert ascii data to binary
%% 01 02 03 .. even number of possibly blank separated hex digits
data_1(Message) ->
    %% remove blanks then convert to binary
    data(binary:replace(Message,<<"\s">>,<<>>,[global])).

decode_ts(<<H1,H2,$:,M1,M2,$:,S1,S2,$:,MS1,MS2,MS3,$:,_US1,_US2,_US3>>) ->
    H = decimal(H1,H2),
    M = decimal(M1,M2),
    S = decimal(S1,S2),
    Ms = decimal(MS1,MS2,MS3),
    %% Us = decimal(US1,US2,US3),
    ((H*60+M)*60+S)*1000 + Ms;
decode_ts(_) ->
    0.

decimal(D1) -> (D1-$0).
decimal(D1,D2) -> (D1-$0)*10 + (D2-$0).
decimal(D1,D2,D3) -> (D1-$0)*100 + (D2-$0)*10 + (D3-$0).
