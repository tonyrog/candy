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
	{print, boolean()} |      %% print message
	{next, mid()} | 	  %% next message to send (after duration)
	{freq, number()} |        %% repeat send in Hz
	{duration, integer()} |   %% time in milliseconds to send
	{repeat, integer()} |     %% Number of frames to send (duration=0)
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
    spawn_link(fun() -> init(Spec) end).

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
	      cancel_send(Send);
	 (_, _) -> ok
      end, S),
    io:format("Traffic stopped\n"),
    stop;
run(_Timer, {send, MID}, S) ->
    send_it(S, maps:get({mid,MID}, S, undefined));
run(_Timer, {stop,MID}, S) ->
    send_stop(S, maps:get({mid,MID}, S, undefined)).


%% send frame
send_it(S, undefined) ->
    {ok, S};
send_it(S, Send) ->
    case maps:get(count, Send, undefined) of
	undefined -> %% not counting timer only
	    send_hz(S, Send);
	0 -> %% last frame stop sending
	    send_stop(S, Send);
	Count when Count > 0 ->
	    send_hz(S, Send#{ count => Count - 1 })
    end.

%% send frame and set repeat timer at freq Hz
send_hz(S, Send) ->
    Freq = maps:get(freq, Send, 1.0),
    Wait = ceil(1000 * (1/Freq)),  %% fixme, make beter
    send(Send),
    MID = maps:get(mid, Send),
    Timer1 = erlang:start_timer(Wait, self(), {send,MID}),
    Send1 = Send#{ timer => Timer1 },
    MID = maps:get(mid, Send),
    S1 = S#{ {mid,MID} => Send1  },
    {ok, S1}.

%% stop sending Send frame, check if there is a next frame to send
send_stop(S, undefined) ->
    {ok, S};
send_stop(S, Send) ->
    cancel_send(Send),
    %% deactive and rester counter
    MID = maps:get(mid, Send),
    Repeat = maps:get(repeat, Send, 0),
    Send1 = Send#{ count => Repeat, 
		   active => false, timer => false },
    S1 = S#{ {mid,MID} => Send1 },
    %% check if there is a next frame to send
    send_next(S1, maps:get(next, Send1, undefined)).

send_next(S, undefined) ->
    {ok, S};
send_next(S, MID) ->
    case maps:get({mid,MID}, S, undefined) of
	undefined -> 
	    {ok, S};
	Send ->
	    Send1 = Send#{ active => true },
	    S1 = S#{ {mid,MID} => Send1 },
	    {ok,S2} = send_duration(S1, Send1),
	    send_it(S2, Send1)
    end.    

send_duration(S, Send) ->
    case maps:get(duration, Send, undefined) of
	undefined -> {ok, S};
	D when is_integer(D), D > 0 ->
	    MID = maps:get(mid, Send),
	    erlang:start_timer(D, self(), {stop,MID}),
	    {ok, S}
    end.

cancel_send(Send) ->
    Timer = maps:get(timer, Send, false),
    try erlang:cancel_timer(Timer) of
	false ->
	    receive
		{timeout, Timer, _} ->
		    ok
	    after 0 ->
		    ok
	    end;
	_Remain ->
	    ok
    catch
	error:_ ->
	    ok
    end.

send(#{print := Print, id := ID, fd := FD, data := Data}) ->
    if Print ->
	    io:format("send ID:~p FD:~p Data:~p~n", [ID, FD, Data]);
       true ->
	    ok
    end,
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
    Duration = proplists:get_value(duration, Options, undefined),
    Repeat = proplists:get_value(repeat, Options, undefined),
    Delay = proplists:get_value(delay, Options, 0),
    ID = proplists:get_value(id, Options, 16#123),
    %% FIXME: add multiple pid chunks
    PID24 = proplists:get_value(pid24, Options, 0),
    %% FIXME: add random data
    Data = proplists:get_value(data, Options, []),
    FD = proplists:get_value(fd, Options, false),
    Data1 = format_data(FD, PID24, Data),
    %% Output
    Print = proplists:get_value(print, Options, false),
    Send = #{ mid => MID,
	      id => ID,
	      active => Active,
	      timer => false,
	      duration => Duration,
	      repeat => Repeat,
	      count => Repeat,
	      fd => FD,
	      pid => PID24,
	      data => Data1,
	      freq => Freq,
	      next => Next,
	      print => Print
	    },
    S1 = S#{ {mid,MID} => Send },
    if not Active -> 
	    load(Spec, I+1, S1);
       true ->
	    if Delay > 0 ->
		    erlang:start_timer(Delay, self(), {send,MID}),
		    load(Spec, I+1, S1);
	       true ->
		    {ok,S2} = send_it(S1, Send),
		    {ok,S3} = send_duration(S2, Send),
		    load(Spec, I+1, S3)
	    end
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
    %% run for 20 seconds
    %% alternating on-frame and off-frame
    %% sending with 1 Hz
    start([{timeout, 20},
	   {send, 
	    [{mid, on},
	     {next, off},
	     {id, 123},
	     {data, <<1,1,1,4,1,1,1,1>>},
	     {freq, 1},
	     {duration, 4000},
	     {print, true}
	    ]},
	   {send,
	    [{mid, off},
	     {active, false},
	     {next, on},
	     {id, 123},
	     {data, <<1,1,1,0,1,1,1,1>>},
	     {freq, 1},
	     {duration, 4000},
	     {print, true}
	    ]},
	   {send,
	    [{mid, two_1},
	     {id, 124},
	     {data, <<2,3,2,3,2,3,2,3>>},
	     {freq, 1},
	     {next, two_2},
	     %% {print, true},
	     {duration, 2000}
	    ]},
	   {send,
	    [{mid, two_2},
	     {id, 124},
	     {data, <<2,1,2,1,2,1,2,1>>},
	     {active, false},
	     {freq, 1},
	     {next, two_1},
	     %% {print, true},
	     {duration, 2000}
	    ]},
	   {send, 
	    [{mid, three_1},
	     {id, 125},
	     {data, <<2,3,2,3,2,3,2,3>>},
	     {freq, 1},
	     {next, three_2},
	     %% {print, true},
	     {duration, 2000}
	    ]},
	   {send, 
	    [{mid, three_2},
	     {id, 125},
	     {data, <<1,3,1,3,1,3,1,3>>},
	     {active, false},
	     {freq, 1},
	     {next, three_1},
	     %% {print, true}
	     {duration, 2000}
	    ]}
	  ]).

mercedes() ->
    FrameID = 16#120,
    _BytePos = 16,
    Bit = 16#04,
    OnData = <<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,Bit>>,
    OffData = <<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0>>,
    Fd = true,
    %% run for 12 seconds
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
