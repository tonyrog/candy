%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Small candy demo
%%% @end
%%% Created :  8 Apr 2021 by Tony Rogvall <tony@rogvall.se>

-module(candy_demo).

-export([start/0]).
-export([loop/0]).

start() ->
    can_udp:start(0, [{fd,true},{ttl,0}]),
    loop().

loop() ->
    lists:foreach(
      fun(FID) ->
	      spawn(
		fun Server() ->
			I = rand:uniform(505)-1, %% 0-504
			can:send(FID, <<0:I, I:8, 0:(504-I)>>),
			timer:sleep(50),
			Server()
		end)
      end, lists:seq(16#700, 16#710)).



    
     
		    
    
