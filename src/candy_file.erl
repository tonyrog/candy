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

	
