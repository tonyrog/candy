%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    Read CAN dump files
%%% @end
%%% Created : 13 Jun 2024 by Tony Rogvall <tony@rogvall.se>

-module(candy_file).
-export([read/1, read/2]).
%% test
-export([read_mersa/0]).
-export([read_mersa2/0]).
-export([print_tp_frames/2]).
-export([diff/1, diff/2, diff_frames/2]).
-include_lib("can/include/can.hrl").

%% example read mersa file for frameid 0x120, pid = 0x0A0E00
%% return data frames where data is replaced with pid fdata
read_mersa() ->
    %% 0x120, 0x0A0E00, 3, 0x04
    %% read mersa CANFD data and print bit at bit pos 28 for frame id 0x120 and pid 0x0A0E00
    FileName = filename:join([code:lib_dir(candy), "data",
			      "MERSA_HEL_PCAN.trc"]),
    read(FileName, #{id=>16#120, pid=>16#0A0E00, pos=>28, time_scale=>1}).

read_mersa2() ->
    %% 0x120, 0x0A0E00, 1, 0x10
    %% read mersa CANFD data and print bit at bit pos 11 for frame id 0x120 and pid 0x0A0E00
    Filename = filename:join([code:lib_dir(candy), "data", 
			      "MERSA2_HEL.trc"]),
    read(Filename, #{id=>16#120, pid=>16#0A0E00, pos=>11, time_scale=>1}).

read(Filename) ->
    read(Filename, #{}).
read(Filename, Opts) when is_list(Opts) ->
    read(Filename, maps:from_list(Opts));
read(Filename, Opts) when is_map(Opts) ->
    candy_play:fold_file(Filename, fun collect_frame/6, Opts).

collect_frame(Frame, _Dir, _Hw, Opts, Time0, Acc) -> 
    case maps:get(time_scale, Opts, 0) of
	0 ->
	    ok; %% no sleep
	TimeScale -> 
	    candy_play:sleep(Frame#can_frame.ts, Time0, TimeScale)
    end,
    case maps:get(id, Opts, undefined) of
	undefined ->
	    case maps:get(pid, Opts, undefined) of
		undefined ->
		    [Frame|Acc];
		Pid ->
		    find_pid_frame(Pid, Frame, Opts, Acc)
	    end;
	ID when ID =:= (?CAN_EFF_MASK band Frame#can_frame.id) -> 
	    case maps:get(pid, Opts, undefined) of
		undefined ->
		    [Frame|Acc];
		Pid ->
		    find_pid_frame(Pid, Frame, Opts, Acc)
	    end;
	_ ->
	    Acc
    end.
	    
find_pid_frame(Pid, Frame, Opts, Acc) ->
    try candy_play:split_pid_data(Frame#can_frame.data) of
	[] ->
	    Acc;
	PidData ->
	    case lists:keyfind(Pid, 1, PidData) of
		{_, Data} ->
		    case maps:get(pos, Opts, undefined) of
			Pos when is_integer(Pos), Pos >= 0 ->
			    case Data of
				<<_:Pos/bitstring, Bit:1, _/bitstring>> ->
				    case Acc of
					[] -> ok;
					[{Bit,_}|_] -> ok;
					[{_,_}|_] ->
					    case Bit of
						1 -> io:format("ON\n");
						0 -> io:format("OFF\n")
					    end
				    end,
				    [{Bit,Frame#can_frame{data=Data}}|Acc];
				_ ->
				    [{0,Frame#can_frame{data=Data}}|Acc]
			    end;
			_ ->
			    [{0,Frame#can_frame{data=Data}}|Acc]
		    end;
		false ->
		    Acc
	    end
    catch
	error:badarg ->
	    Acc
    end.

-define(SINGLE, 0).
-define(FIRST,  1).
-define(NEXT,   2).
-define(FLOW,   3).

	
print_tp_frames(Type, [F | Fs]) ->
    print_tp_frame(Type,F),
    print_tp_frames(Type,Fs);
print_tp_frames(_Type,[]) ->
    ok.

print_tp_frame(Type,#can_frame{ id=ID, data=Data }) ->
    io:put_chars(candy_record:format_id(ID)),
    Ms = split_messages(Type,Data),
    lists:foreach(
      fun({Header, MsData}) ->
	      io:format("  ~p: ~s\n", [Header, 
				       candy_record:format_data(MsData)])
      end, Ms).

split_messages(Type=fd, <<?SINGLE:4,0:4,Size,Data:Size/binary, Tail/binary>>) ->
    %% ISO 15765-2 (Size 0..62)
    [{{'SF2',Size}, Data} | split_messages(Type, Tail)];
split_messages(Type, <<?SINGLE:4,Size:4, Data:Size/binary, Tail/binary>>) ->
    %% ISO 15765-1 (Size 1..7)
    [{{'SF1',Size}, Data} | split_messages(Type,Tail)];
split_messages(Type=fd, <<?FIRST:4,0:12,Size:32,Data0/binary>>) ->
    %% ISO 15765-2 (Size 0..4Gb)
    Size0 = byte_size(Data0),
    if Size0 >= Size -> %% single frame really?
	    <<Data:Size/binary, Tail/binary>> = Data0,
	    [{{'FF2',Size}, Data} | split_messages(Type, Tail)];
       true ->
	    [{{'FF2',Size}, Data0}]
    end;
split_messages(Type, <<?FIRST:4,Size:12,Data0/binary>>) ->
    Size0 = byte_size(Data0),
    if Size0 >= Size -> %% single frame really?
	    <<Data:Size/binary, Tail/binary>> = Data0,
	    [{{'FF1',Size}, Data} | split_messages(Type, Tail)];
       true ->
	    [{{'FF1',Size}, Data0}]
    end;
split_messages(_, <<?NEXT:4,Index:4,Data/binary>>) ->
    [{{'CF',Index},Data}];
split_messages(_, <<?FLOW:4,Flag:4,Bs,St,Pad/binary>>) ->
    %% Bs = Blocksize
    %% St = minimym separation time
    %% 0-127 = 0 - 127 ms
    %% 0xf1 - 0xf9 = 100 us - 900 us
    [{{'FC',Flag,Bs,St},Pad}];
split_messages(_, <<T:4,F:4,Data/binary>>) ->
    [{{'??',T,F},Data}];
split_messages(_, <<>>) ->
    [].

%%
%% Read CAN data sort it according to ID band 16#1fffffff
%%
%% For each ID
%%
%% Options = 
%%    mask        = 0     - match mask
%%    id          = 0     - match id
%%    print_zero  = true  - print zero diff
%%    tdiff       = 0     - min diff in ms between frames
%%    bytepos     = false - byte position
%%    bitmask     = 0     - bitmask in byte
%%
%%  filter(frame) = (frame.fid & mask) == (id & mask)
%%  mask = 0 => all frames (default!)
%%  maybe change this if id != 0 ? why else give id as input?
%%
diff(Filename) ->
    diff(Filename, #{}).
diff(Filename, Opts) when is_map(Opts) ->
    Fs = read(Filename, Opts),
    diff_frames(Fs, Opts).

diff_frames(Fs, Opts) ->
    Mask = maps:get(mask, Opts, 0),
    Match = maps:get(id, Opts, 0),
    Fs1 = filter_frames(Fs, Mask, Match),
    Fs2 = lists:sort(fun(#can_frame{id=ID1}, #can_frame{id=ID2}) ->
			     (ID1 band ?CAN_EFF_MASK) < (ID2 band ?CAN_EFF_MASK)
		     end, Fs1),
    diff_frames_(Fs2, Opts).

diff_frames_([], _Opts) ->
    ok;
diff_frames_([#can_frame{id=ID, data=Data, ts=Ts}|Fs], Opts) ->
    io:put_chars([candy_record:format_id(ID),":\n"]),
    diff_frames_(ID, Data, Ts, Fs, Opts).

diff_frames_(ID1, Data1, Ts1, [#can_frame{id=ID1, data=Data2, ts=Ts2}|Fs], Opts) ->
    {Sum,Data3} = xor_data(Data1, Data2),
    Pos = maps:get(bytepos, Opts, false),
    PrintPos = 
	if is_integer(Pos) ->
		if (byte_size(Data1) >= Pos+1) andalso (byte_size(Data2) >= Pos+1) ->
			BitMask = maps:get(bitmask, Opts, 0),
			(binary:at(Data1,Pos) band BitMask) =/=
			    (binary:at(Data2,Pos) band BitMask);
		   true ->
			false
		end;
	   true -> false
	end,
    PrintZero = maps:get(print_zero, Opts, true),
    TDiff = maps:get(tdiff, Opts, 0.0),
    if (Sum =:= 0) andalso not PrintZero ->
	    %% do not print zero diff! (pass old timestamp)
	    diff_frames_(ID1, Data2, Ts1, Fs, Opts);
       (Ts2-Ts1) =< TDiff ->
	    diff_frames_(ID1, Data2, Ts2, Fs, Opts);  %% to short time between diffs
       PrintPos; is_integer(Pos) ->
	    Data4 = <<(binary:at(Data2,Pos))>>,
	    Ti = if is_float(Ts2) -> trunc(Ts2); Ts2 =:= ?CAN_NO_TIMESTAMP -> 0 end,
	    Tstr = tl(integer_to_list(Ti+1000000)),
	    io:put_chars(["  ", Tstr, " ", candy_record:format_data(Data4), "\n"]),
	    diff_frames_(ID1, Data2, Ts2, Fs, Opts);
       true ->
	    Ti = if is_float(Ts2) -> trunc(Ts2); Ts2 =:= ?CAN_NO_TIMESTAMP -> 0 end,
	    Tstr = tl(integer_to_list(Ti+1000000)),
	    io:put_chars(["  ", Tstr, " ", candy_record:format_data(Data3), "\n"]),
	    diff_frames_(ID1, Data2, Ts2, Fs, Opts)
    end;
diff_frames_(_ID1, _Data1, _Ts1, Fs, Opts) ->
    diff_frames_(Fs, Opts).

filter_frames([F=#can_frame{id=ID, data=_Data}|Fs], Mask, Match) ->
    if (ID band Mask =:= (Match band Mask)) ->
	    [F | filter_frames(Fs, Mask, Match)];
       true ->
	    filter_frames(Fs, Mask, Match)
    end;
filter_frames([], _, _) ->
    [].
    

xor_data(A, B) ->
    xor_data(A, B, 0, []).

xor_data(<<A,ABin/binary>>, <<B,BBin/binary>>, Sum, Acc) ->
    C = A bxor B,
    xor_data(ABin, BBin, C+Sum, [C|Acc]);
xor_data(<<>>, <<B,BBin/binary>>, Sum, Acc) ->
    xor_data(<<>>, BBin, B+Sum, [B|Acc]);
xor_data(<<A,ABin/binary>>, <<>>, Sum, Acc) ->
    xor_data(ABin, <<>>, A+Sum, [A|Acc]);
xor_data(<<>>, <<>>, Sum, Acc) ->
    {Sum, list_to_binary(lists:reverse(Acc))}.
