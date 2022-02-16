%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Small candy demo
%%% @end
%%% Created :  8 Apr 2021 by Tony Rogvall <tony@rogvall.se>

-module(candy_demo).

-export([start/0]).
-export([loop/0]).

-define(FIRST, 16#701).
-define(LAST,  16#710).
-define(OFF, 0).
-define(ON,  16#ff).

start() ->
    can_udp:start(0, [{fd,true},{ttl,0}]),
    setup(),
    loop().

loop() ->
    lists:foreach(
      fun(FID) ->
	      horizontal(1,10,FID, ?OFF, ?ON),
	      timer:sleep(200)
      end, lists:seq(?FIRST, ?LAST)),
    lists:foreach(
      fun(FID) ->
	      horizontal(1,10,FID, ?OFF, ?ON),
	      timer:sleep(200)
      end, lists:seq(?LAST, ?FIRST, -1)),
    timer:sleep(300),
    lists:foreach(
      fun(X) ->
	      vertical(X, ?FIRST, ?LAST, ?OFF, ?ON),
	      timer:sleep(200)
      end, lists:seq(1, 10)),
    lists:foreach(
      fun(X) ->
	      vertical(X, ?FIRST, ?LAST, ?OFF, ?ON),
	      timer:sleep(200)
      end, lists:seq(9, 1, -1)),

    random_plot(100),
    
    loop().

random_plot(0) ->    
    ok;
random_plot(I) -> 
    X = uniform(1,10),
    Y = uniform(?FIRST, ?LAST),
    plot(X, Y, ?OFF, ?ON),
    timer:sleep(100),
    random_plot(I-1).

random(FID1,FID2) ->
    F = uniform(FID1,FID2),
    I = rand:uniform(505)-1, %% 0-504
    can:send(F, <<0:I, I:8, 0:(504-I)>>),
    timer:sleep(50).


uniform(A,B) when A =< B ->
    N = B-A+1,
    A + (rand:uniform(N)-1).

ite(Cond,A,_B) when Cond -> A;
ite(_Cond,_A,B) -> B.

within(X, A, B) -> (X >= A) andalso (X =< B).

horizontal(X1,X2,Y,Z,V) ->
    V1 = ite(within(1,X1,X2), V, Z),
    V2 = ite(within(2,X1,X2), V, Z),
    V3 = ite(within(3,X1,X2), V, Z),
    V4 = ite(within(4,X1,X2), V, Z),
    V5 = ite(within(5,X1,X2), V, Z),
    V6 = ite(within(6,X1,X2), V, Z),
    V7 = ite(within(7,X1,X2), V, Z),
    V8 = ite(within(8,X1,X2), V, Z),
    V9 = ite(within(9,X1,X2), V, Z),
    V10 = ite(within(10,X1,X2), V, Z),
    can:send(Y, <<0:64,
		  V1:32,V2:32,V3:32,V4:32,V5:32,V6:32,
		  V7:64, V8:64, V9:64, V10:64>>).

vertical(X,Y1,Y2,Z,V) ->
    lists:foreach(fun(Y) -> plot(X,Y,Z,V) end, lists:seq(Y1,Y2)).

plot(X, Y, Z, V) when X >= 1, X =< 6 ->
    can:send(Y, <<0:64, 
		  Z:((X-1)*32),V:32,Z:((6-X)*32),
		  Z:(4*64)>>);
plot(X, Y, Z, V) when X >= 7, X =< 10 ->
    can:send(Y, <<0:64,
		  Z:(6*32),
		  Z:((X-7)*64),V:64,Z:((10-X)*64)>>).

setup() ->
    lists:foreach(
      fun(FID) ->
	      can:send(FID, <<0:512>>)
      end, lists:seq(?FIRST, ?LAST)).
    
