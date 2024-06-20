%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2019, Tony Rogvall
%%% @doc
%%%    Play CAN logfile (with filestamp)
%%% @end
%%% Created :  9 Mar 2019 by Tony Rogvall <tony@rogvall.se>

-module(candy_play).

-export([file/1, file/2, file/3]).
-export([fold_file/3, fold_fd/5]).
-export([current_time/0]).
-export([sleep/3]).
-export([split_pid_data/1]).
-export([mersa/1]).

-include_lib("can/include/can.hrl").

-compile(export_all).

file(File) -> file(File,0,1).
file(File,Repeat) -> file(File,Repeat,1).

file(File,Repeat,TimeScale) when is_integer(Repeat), Repeat >= -1,
				 is_number(TimeScale), 
				 TimeScale > 0 ->
    (catch error_logger:tty(false)),
    %% can_udp:start(0, [{ttl,0}]),
    fold_file(File, fun play/6, #{repeat => Repeat, time_scale => TimeScale}).


fold_file(File, Play, Params) ->	      
    case file:open(File,[read,binary,raw]) of
	{ok,Fd} ->
	    try fold_fd(Fd, Play, [], current_time(), Params) of
		Result -> Result
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.

%% callback to playback a rx frame followed by a delay
play(Frame, rx, _Hw, Params, Time0, Acc) -> 
    %% _Vsn = maps:get(vsn, Params, {0,0}),
    %% io:format("~s/~w: ~s ~s\n",
    %%    [_Hw,_Vsn,rx,can_probe:format_frame(Frame)]),
    TimeScale = maps:get(time_scale, Params, 1),
    sleep(Frame#can_frame.ts, Time0, TimeScale),
    can:sync_send(Frame#can_frame{intf=0,ts=0}),
    Acc;
play(_Frame, _Dir, _Hw, _Params, _Time0, Acc) -> 
    Acc.

sleep(TimeStamp, StartTime, Scale) ->
    TimeOffsUs = max(0, (current_time() - StartTime)),
    Delta = 
	if TimeStamp =:= 0 ->
		0;
	   true ->
		trunc((1000*TimeStamp*Scale - TimeOffsUs)/1000)
	end,
    if Delta > 0 ->
	    %% io:format("  td = ~w\n", [Delta]),
	    timer:sleep(Delta);
       true ->
	    ok
    end.
    

mersa(File) ->
    mersa(File, #{}).

mersa(File, Params) ->
    fold_file(File, fun mersa_print/6, Params#{repeat => 0}).

mersa_print(Frame, rx, _Hw, Params, _Time0, _Acc) ->
    Data = Frame#can_frame.data,
    ID = Frame#can_frame.id band ?CAN_EFF_MASK,
    if Frame#can_frame.id band ?CAN_FD_FLAG =/= 0 ->
	    case match_id(ID, Params) of
		true ->
		    try split_pid_data(Data) of
			PidData ->
			    case match_pid(PidData, Params) of
				true ->
				    io:format("~06w: ID  : ~s [~w]\n", 
					      [ms(Frame#can_frame.ts), 
					       integer_to_list(ID, 16),
					       Frame#can_frame.len]),
				    format_pid_data(PidData,16);
				{true,PidData1} ->
				    io:format("~06w: ID  : ~s [~w]\n", 
					      [ms(Frame#can_frame.ts), 
					       integer_to_list(ID, 16),
					       Frame#can_frame.len]),
				    format_pid_data(PidData1,2);
				false ->
				    ok
			    end
		    catch
			_:_ ->
			    io:format("~06w: ID  : ~s [~w]\n", 
				      [ms(Frame#can_frame.ts), 
				       integer_to_list(ID, 16),
				       Frame#can_frame.len]),
			    io:format("****| ~s\n", [binary:encode_hex(Data)])
		    end;
		false ->
		    ok
	    end;
       true ->
	    case match_id(ID, Params) of
		true ->
		    io:format("~06w: ID  : ~s [~w]\n", 
			      [ms(Frame#can_frame.ts), 
			       integer_to_list(ID, 16),
			       Frame#can_frame.len]),
		    io:format("    | ~s\n", [binary:encode_hex(Data)]);
		false ->
		    ok
	    end
    end;
mersa_print(_Frame, _Dir, _Hw, _Params, _Time0, Acc) ->
    Acc.

ms(Ts) -> 
    trunc(Ts).
    

match_id(ID, Params) ->
    case maps:get(id, Params, undefined) of
	undefined -> true;
	ID -> true;
	IDList when is_list(IDList) -> lists:member(ID, IDList);
	_ -> false
    end.

match_pid([PD={Pid,_Data}|PDs],Params) ->
    case maps:get(pid, Params, undefined) of
	undefined -> true;
	Pid -> {true,[PD]};
	PidList when is_list(PidList) -> 
	    case lists:member(Pid, PidList) of
		true -> {true,[PD]};
		false -> match_pid(PDs, Params)
	    end;
	_ -> match_pid(PDs, Params)
    end;
match_pid([],_Params) ->
    false.


format_pid_data([{PID,Data}|Rest],Base) ->
    <<I3,I2,I1>> = <<PID:24>>,
    %% J1939-21 PDU format ???
    %% TOS=Type of service, TL=Trailer length, DP=Data page,
    %% PDUF/PF=PDU format, PDUS/PS=PDU specific
    %% <<TOS:3,TL:3,DP:2,PF:8,PS:8>> = <<PID:24>>,
    %% PIDFmt=io_lib:format("tos=~3.2.0B,tl=~w,dp=~w,pf=~w,ps=~2.16.0B",
    %%   [TOS,TL,DP,PF,PS]),
    DataFormat = case Base of
		     16 -> encode_hex(Data);
		     2 -> encode_bin(Data)
		 end,
    io:format("    | ~2.16.0B~2.16.0B~2.16.0B [~w] ~s\n", 
	      [I3,I2,I1,byte_size(Data),DataFormat]),
    format_pid_data(Rest,Base);
format_pid_data([],_Base) ->
    ok.

encode_bin(Data) ->
    [(I+$0) || <<I:1>> <= Data].

encode_hex(Data) ->
    [tl(integer_to_list(I+16#100)) || <<I>> <= Data].


split_pid_data(<<0:24,_/binary>>) ->
    [];
split_pid_data(<<PID:24,DLC,Data:DLC/binary,Rest/binary>>) ->
    [{PID,Data}|split_pid_data(Rest)];
split_pid_data(<<>>) -> [];
split_pid_data(<<0>>) -> [];
split_pid_data(<<0,0>>) ->  [].

current_time() ->
    erlang:system_time(microsecond).

fold_fd(Fd, Fun, Acc, Time0, Params) ->
    case file:read_line(Fd) of
	eof ->
	    R = maps:get(repeat, Params, 0),
	    if R =:= -1 ->
		    file:position(Fd, 0),
		    fold_fd(Fd, Fun, Acc, current_time(), Params);
	       R =:= 0 ->
		    Acc;
	       R > 0 ->
		    file:position(Fd, 0),
		    fold_fd(Fd, Fun, Acc, current_time(),
			    Params#{ repeat => R-1 })
	    end;
	{ok,Line} ->
	    case decode(Line,Params) of
		ignore ->
		    fold_fd(Fd, Fun, Acc, Time0, Params);
		{set,KVs} ->
		    Params2 = lists:foldl(fun({K,V}, Params1) ->
						  Params1#{ K => V }
					  end, Params, KVs),
		    %% io:format("set: ~p\n", [Params2]),
		    fold_fd(Fd, Fun, Acc, Time0, Params2);
		{ok,Hw,Dir,Frame=#can_frame {}} ->
		    Acc1 = Fun(Frame, Dir, Hw, Params, Time0, Acc),
		    fold_fd(Fd, Fun, Acc1, Time0, Params);
		{error,Reason} ->
		    io:format("error: ~p, line ~p\n", [Reason,Line]),
		    timer:sleep(2000),
		    fold_fd(Fd, Fun, Acc, Time0, Params)
	    end
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
decode(<<"Dearborn ", _/binary>>,_) -> ignore;
decode(<<"--------", _/binary>>,_) -> ignore;
decode(<<"Created ", _/binary>>,_) -> ignore;
decode(<<"Timestamp ", _/binary>>,_) -> ignore;
decode(<<"\n">>,_) -> ignore;
decode(<<"Msg ID ", _/binary>>,_) -> ignore;
decode(<<";$FILEVERSION=",A,".",B,_/binary>>,_) -> 
    {set,[{hw,pcan},{vsn,{A-$0,B-$0}}]};
%%decode(<<";$FILEVERSION=1.2",_/binary>>,_) -> {set,[{hw,pcan},{vsn,{1,2}}]};
%%decode(<<";$FILEVERSION=1.3",_/binary>>,_) -> {set,[{hw,pcan},{vsn,{1,3}}]};
%%decode(<<";$FILEVERSION=2.0",_/binary>>,_) -> {set,[{hw,pcan},{vsn,{2,0}}]};
%%decode(<<";$FILEVERSION=2.1",_/binary>>,_) -> {set,[{hw,pcan},{vsn,{2,1}}]};
decode(<<";COLUMNS=",Col/binary>>,_) -> {set,[{cols,split_col(Col)}]};
decode(<<";", _/binary>>,_) -> ignore;   %% PCAN comment
%% CANUSB $T
decode(<<$T,ID29:8/binary,L,Tail/binary>>,_State) ->
    ID = binary_to_integer(ID29,16),
    Len = binary_to_integer(<<L>>, 16),
    case Tail of
	<<Message:Len/binary-unit:16, Ts:4/binary,$\n>> ->
	    Stamp = binary_to_integer(Ts, 16),
	    {ok, canusb, rx, 
	     can:create(ID,Len,true,false,0,data(Message),Stamp)};
	<<Message:Len/binary-unit:16,$\n>> ->
	    {ok, canusb, rx,
	     can:create(ID,Len,true,false,0,data(Message))};
	_ ->
	    {error, bad_format}
    end;
%% CANUSB $t
decode(<<$t,ID11:3/binary,L,Tail/binary>>,_State) ->
    ID = binary_to_integer(ID11,16),
    Len = binary_to_integer(<<L>>, 16),
    case Tail of
	<<Message:Len/binary-unit:16, Ts:4/binary, $\n>> ->
	    Stamp=binary_to_integer(Ts, 16),
	    {ok, canusb, rx, 
	     can:create(ID,Len,false,false,0,data(Message),Stamp)};
	<<Message:Len/binary-unit:16, $\n>> ->
	    {ok, canusb, rx,
	     can:create(ID,Len,false,false,0,data(Message))};
	_ ->
	    {error, bad_format}
    end;
decode(Line, State) ->
    Parts = case re:split(Line, "[\t\s\n]+", [trim]) of
		[<<>>|LParts] -> LParts; %% skip leading blans
		Parts0 -> Parts0
	    end,
    Hw = maps:get(hw, State, undefined),
    Vsn = maps:get(vsn, State, undefined),
    case {Hw,Vsn,Parts} of
	{pcan,Vsn,_} ->
	    DefaultCols = 
		case Vsn of
		    {1,1} -> [$N,$O,$d,$I,$l,$D];
		    {1,2} -> [$N,$O,$B,$d,$I,$l,$D];
		    {1,3} -> [$N,$O,$B,$d,$I,$R,$l,$D];
		    {2,0} -> [$N,$O,$T,$I,$d,$l,$D];
		    {2,1} -> [$N,$O,$T,$B,$I,$d,$R,$L,$D];
		    _ -> [$N,$O,$d,$I,$l,$D]  %% default to 1.1 format
		end,
	    Cols = maps:get(cols, State, DefaultCols),
	    pcan_create(Cols, Parts);
	{_,_,[ID0, Time, <<"CH#",_N>>, <<L>> | Rest]} ->
	    Len = binary_to_integer(<<L>>, 16),
	    case collect_message(Len, Rest, <<>>) of
		{Data, [<<"RX">>]} ->
		    ID = binary_to_integer(ID0,16),
		    Ext = (ID > ?CAN_SFF_MASK),  %% must be extended
		    Stamp = decode_ts(Time),
		    {ok, vbox, rx, can:create(ID,Len,Ext,false,0,Data,Stamp)};
		_ ->
		    {error,bad_format}
	    end;
	_ ->
	    {error, bad_format}
    end.

split_col(Col) ->
    [X || <<X>> <- string:split(Col, ",", all)].

pcan_create(Cols, Es) ->
    pcan_create(Cols, Es, #can_frame{id=0}).

pcan_create(Cols, Es, Frame) ->
    %% io:format("Cols=~p, parts=~p\n", [Cols,Es]),
    pcan_cols(Cols, Es, undefined, Frame).

pcan_cols([$N|Cols], [_Num|Es], Dir, Frame) ->
    pcan_cols(Cols, Es, Dir, Frame);
pcan_cols([$O|Cols], [Time|Es], Dir, Frame) ->
    TOffs = binary_to_float(Time),
    pcan_cols(Cols, Es, Dir, Frame#can_frame{ts=TOffs});
pcan_cols([$T|Cols], [Type|Es], Dir, Frame) ->
    case Type of
	<<"DT">> ->
	    pcan_cols(Cols, Es, Dir, Frame);
	<<"FD">> ->
	    pcan_cols(Cols,Es,Dir,Frame#can_frame { id = ?CAN_FD_FLAG });
	<<"FB">> -> %% FD, bitrate switch
	    pcan_cols(Cols,Es,Dir,Frame#can_frame { id = ?CAN_FD_FLAG });
	<<"FE">> -> %% FD with error bit
	    pcan_cols(Cols,Es,Dir,Frame#can_frame { id = ?CAN_FD_FLAG bor ?CAN_ERR_FLAG });
	<<"BI">> -> %% FD, bitrate switch and error
	    pcan_cols(Cols,Es,Dir,Frame#can_frame { id = ?CAN_FD_FLAG bor ?CAN_ERR_FLAG });	    
	<<"RR">> ->
	    pcan_cols(Cols,Es,Dir,Frame#can_frame { id = ?CAN_RTR_FLAG });
	<<"ST">> -> %% Hardware status change
	    pcan_cols(tl(Cols),Es,Dir,Frame);
	<<"EC">> -> %% Error counter changed, error frame? (warning)
	    pcan_cols(tl(Cols),Es,Dir,Frame);
	<<"ER">> -> %% Error frame
	    pcan_cols(tl(Cols),Es,Dir,Frame#can_frame { id = ?CAN_ERR_FLAG });
	_ -> 
	    pcan_cols(tl(Cols),Es,Dir,Frame)
    end;
pcan_cols([$B|Cols], [Bus|Es],Dir,Frame) ->
    if Bus =:= <<"-">> ->
	    pcan_cols(Cols,Es,Dir,Frame);
       true ->
	    _Intf = binary_to_integer(Bus), 
	    pcan_cols(Cols,Es,Dir,Frame)
    end;
pcan_cols([$I|Cols], [ID0|Es],Dir,Frame) ->
    if ID0 =:= <<"-">> ->
	    pcan_cols(Cols,Es,Dir,Frame);
       true ->
	    ID = binary_to_integer(ID0,16),
	    Ext = if (ID > ?CAN_SFF_MASK) -> ?CAN_EFF_FLAG;
		     true -> 0
		  end,
	    ID1 = Frame#can_frame.id + ID + Ext, %% flags set before
	    pcan_cols(Cols,Es,Dir,Frame#can_frame{id=ID1})
    end;
pcan_cols([$d|Cols], [E|Es],_Dir,Frame) ->
    case E of
	<<"Rx">> ->
	    pcan_cols(Cols,Es,rx,Frame);
	<<"Tx">> ->
	    pcan_cols(Cols,Es,tx,Frame);
	_ ->
	    ignore
    end;
pcan_cols([$R|Cols],[_|Es],Dir,Frame) ->
    pcan_cols(Cols,Es,Dir,Frame);
pcan_cols([$l|Cols],[Len0|Es],Dir,Frame) -> %% actual length
    Len = binary_to_integer(Len0,10),
    pcan_cols(Cols,Es,Dir,Frame#can_frame{len=Len});
pcan_cols([$L|Cols],[Len0|Es],Dir,Frame) -> %% length code!!
    Len = binary_to_integer(Len0,10),
    pcan_cols(Cols,Es,Dir,Frame#can_frame{len=Len});
pcan_cols([$D],[<<"RTR">>],Dir,Frame) ->
    ID = Frame#can_frame.id + ?CAN_RTR_FLAG,
    {ok,pcan,Dir,Frame#can_frame{id=ID}};
pcan_cols([$D], [],Dir,Frame) 
  when ?CAN_RTR_FLAG band Frame#can_frame.id =/= 0 ->
    {ok,pcan,Dir,Frame#can_frame{data = <<>>}};
pcan_cols([$D],Es,Dir,Frame) ->
    %% fixme?: check if len is actual format $l or coded format $L
    %% now we just collect rest of data 
    Len = -1, %% Frame#can_frame.len
    case collect_message(Len, Es, <<>>) of
	{Data, []} ->
	    {ok, pcan, Dir,Frame#can_frame{data=Data}};
	{_Data, Es1} ->
	    {error,{bad_format,Es1}}
    end.

collect_message(L, [], Data) when L < 0 ->   {Data, []};
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
