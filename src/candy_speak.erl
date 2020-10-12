%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    CandySpeak parse/compile
%%% @end
%%% Created : 12 Oct 2020 by Tony Rogvall <tony@rogvall.se>

-module(candy_speak).

-export([tokens/1]).
-export([parse/1, parse/2]).


parse(String) ->
    parse("*internal*", String).

parse(File, String) ->
    parse(File, String, #{}).

parse(File, Binary, Opts) when is_binary(Binary) ->
    parse(File, binary_to_list(Binary), Opts);    
parse(File, String, _Opts) ->
    case tokens(String) of
	{ok,Ts} ->
	    case candy_speak_gram:parse(Ts) of
		{ok,Statments} ->
		    {ok,Statments};
		Error ->
		    io:format("~s: Error: ~p\n", [File,Error]),
		    Error
	    end;
	Error={error,{Ln,Mod,Why}} when 
	      is_integer(Ln), is_atom(Mod) ->
	    Reason = Mod:format_error(Why),
	    io:format("~s:~w: ~s\n", [File,Ln,Reason]),
	    Error;
	Error ->
	    io:format("~s: Error: ~p\n", [File,Error]),
	    Error
    end.


tokens(String) ->
    case candy_speak_lex:string(remove_comments(String)) of
	{ok,Ts,_Ln} -> {ok,Ts};
	Error -> Error
    end.

%% remove C-style comments from data
remove_comments([$/,$/|Cs]) -> remove_comments(remove_line(Cs));
remove_comments([$/,$*|Cs]) -> remove_comments(remove_block(Cs));
remove_comments([C|Cs]) -> [C|remove_comments(Cs)];
remove_comments([]) -> [].

%% remove until */ but keep all \n
remove_block([$*,$/|Cs]) -> Cs;
remove_block([$\n|Cs]) -> [$\n|remove_block(Cs)];
remove_block([_|Cs]) -> remove_block(Cs);
remove_block([]) -> [].

%% remove until end-of-line (but keep it)
remove_line(Cs=[$\n|_]) -> Cs;
remove_line([_|Cs]) -> remove_line(Cs);
remove_line([]) -> [].
