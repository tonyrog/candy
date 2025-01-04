%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%    CAN traffic simulator
%%%    Send carious CAN messages to the CAN bus
%%%    {send_message, send_options()}
%%% @end
%%% Created :  3 Jan 2025 by Tony Rogvall <tony@rogvall.se>

-module(candy_traffic).

-export([start/1]).
-export([init/1, load/1,  run/3]).
-export([send/1]).
-export([format_data/3]).

-export([test/0]).
-export([mercedes/0]).

-type mid() :: term().

-type send_option() ::
	{mid, mid()} |            %% message id
	{active, boolean()} |     %% active or passive
	{next, mid()} | 	  %% next message to send (after duration)
	{freq, integer()} |       %% repeat send in Hz
	{duration, integer()} |   %% time in milliseconds to send
	{delay, integer()} |      %% delay in milliseconds before first send
	{id, integer()} |         %% CAN message id
	{pid24, integer()} |      %% CAN message pid (0 = not used)
	{fd, boolean()} |         %% CAN message is FD
	{data, iolist()}.         %% CAN message data (0-64 bytes in length)

-type traffic_option() :: 
	{timeout, integer()} | 
	{send, [send_option()]}.

-type traffic_spec() :: [traffic_option()].


-spec start(Spec::traffic_spec()) -> ok.
start(Spec) ->
    application:ensure_all_started(can),
    spawn(fun() -> init(Spec) end).

init(Spec) ->
    can_router:attach(),
    S = load(Spec),
    loop(S).

loop(S) ->
    %% io:format("loop ~p\n", [S]),
    receive
	{timeout, Timer, What} ->
	    case run(Timer, What, S) of
		stop -> ok;
		{ok, S1} -> loop(S1)
	    end
    end.

run(_Timer, terminate, S) ->
    maps:foreach(
      fun({mid,_MID}, Send) ->
	      catch erlang:cancel_timer(maps:get(timer, Send, false));
	 (_, _) -> ok
      end, S),
    io:format("Traffic stopped\n"),
    stop;
run(_Timer, {send, MID}, S) ->
    case maps:get({mid,MID}, S, undefined) of
	undefined -> {ok, S};
	Send ->
	    Freq = maps:get(freq, Send, 1.0),
	    Wait = ceil(1000 * (1/Freq)),  %% fixme, make beter
	    send(Send),
	    Timer1 = erlang:start_timer(Wait, self(), {send,MID}),
	    Send1 = Send#{ timer => Timer1 },
	    S1 = S#{ {mid,MID} => Send1  },
	    {ok, S1}
    end;
run(_Timer, {stop,MID}, S) ->
    case maps:get({mid,MID}, S, undefined) of
	undefined -> {ok, S};
	Send ->
	    catch erlang:cancel_timer(maps:get(timer, Send, false)),
	    Send1 = Send#{ active => false, timer => false },
	    S1 = S#{ {mid,MID} => Send1 },
	    case maps:get(next, Send1, undefined) of
		undefined -> {ok, S1};
		NEXT ->
		    case maps:get({mid,NEXT}, S1, undefined) of
			undefined -> {ok, S1};
			Send2 ->
			    Send3 = Send2#{ active => true },
			    S2 = S1#{ {mid,NEXT} => Send3 },
			    Duration = maps:get(duration, Send3, 1),
			    erlang:start_timer(Duration, self(), {stop,NEXT}),
			    run(false, {send, NEXT}, S2)
		    end
	    end
    end.

send(#{id := ID, fd := FD, data := Data}) ->
    io:format("send ID:~p FD:~p Data:~p~n", [ID, FD, Data]),
    if FD =:= true ->
	    can:send_fd(ID, Data);
       true ->
	    can:send(ID, Data)
    end.
    

load(Spec) ->
    load(Spec, 1, #{}).

load([], _I, S) -> S;
load([{timeout, Time}|Spec], I, S) ->
    Timer = erlang:start_timer(Time * 1000, self(), terminate),
    load(Spec, I, S#{ Timer => terminate });
load([{send, Options}|Spec], I, S) ->
    MID = proplists:get_value(mid, Options, I),
    Active = proplists:get_value(active, Options, true),
    Next = proplists:get_value(next, Options, undefined),
    Freq = proplists:get_value(freq, Options, 1.0),
    Duration = proplists:get_value(duration, Options, 1),
    Delay = proplists:get_value(delay, Options, 0),
    ID = proplists:get_value(id, Options, 16#123),
    %% FIXME: add multiple pid chunks
    PID24 = proplists:get_value(pid24, Options, 0),
    %% FIXME: add random data
    Data = proplists:get_value(data, Options, []),
    FD = proplists:get_value(fd, Options, false),
    Data1 = format_data(FD, PID24, Data),
    Send = #{ id => ID,
	      active => Active,
	      timer => false,
	      duration => Duration,
	      fd => FD,
	      pid => PID24,
	      data => Data1,
	      freq => Freq,
	      next => Next
	    },
    if not Active -> 
	    load(Spec, I+1, S#{ {mid,MID} => Send });
       true ->
	    Timer = 
		if Delay > 0 ->
			erlang:start_timer(Delay, self(), {send,MID});
		   true ->
			Wait = ceil(1000 * (1/Freq)),  %% fixme, make beter
			send(Send),
			erlang:start_timer(Wait, self(), {send,MID})
		end,
	    Send1 = Send#{ timer => Timer },
	    erlang:start_timer(Duration, self(), {stop, MID}),
	    S1 = S#{ {mid,MID} => Send1 },
	    load(Spec, I+1, S1)
    end.

format_data(false, PID24, Data) ->
    Data1 = iolist_to_binary(Data),
    Len = byte_size(Data1),
    if PID24 =:= 0, Len > 8 ->
	    error({data_to_long, Len});
       PID24 =:= 0 ->
	    Data1;
       Len > 4 ->
	    error({data_to_long, Len});
       true ->
	    <<PID24:24, Len:8, Data1/binary>>
    end;
format_data(true, PID24, Data) ->
    Data1 = iolist_to_binary(Data),
    Len = byte_size(Data1),
    if PID24 =:= 0, Len > 64 ->
	    error({fd_data_to_long, Len});
       PID24 =:= 0 ->
	    Data1;
       Len > 60 ->
	    error({fd_data_to_long, Len});
       true ->
	    <<PID24:24,Len:8, Data1/binary>>
    end.

test() ->
    %% run for 10 seconds
    %% alternating on-frame and off-frame
    %% sending with 1 Hz
    start([{timeout, 12},
	   {send, 
	    [{mid, on},
	     {next, off},
	     {id, 123},
	     {data, <<1,2,3,4,5,6,7,8>>},
	     {freq, 1},
	     {duration, 4000}
	    ]},
	   {send, 
	    [{mid, off},
	     {active, false},
	     {next, on},
	     {id, 123},
	     {data, <<8,7,6,5,4,3,2,1>>},
	     {freq, 1},
	     {duration, 4000}
	    ]}
	  ]).

mercedes() ->
    FrameID = 16#120,
    BytePos = 16,
    Bit = 16#04,
    OnData = <<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,Bit>>,
    OffData = <<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0>>,
    Fd = true,
    %% run for 10 seconds
    %% alternating on-frame and off-frame
    %% sending with 1 Hz
    start([{timeout, 12},
	   {send, 
	    [{mid, on},
	     {next, off},
	     {id, FrameID},
	     {fd, Fd},
	     {data, OnData},
	     {freq, 1},
	     {duration, 4000}
	    ]},
	   {send, 
	    [{mid, off},
	     {active, false},
	     {next, on},
	     {id, FrameID},
	     {fd, Fd},
	     {data, OffData},
	     {freq, 1},
	     {duration, 4000}
	    ]}
	  ]).
