%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    CandySpeak parse/compile
%%% @end
%%% Created : 12 Oct 2020 by Tony Rogvall <tony@rogvall.se>

-module(candy_speak).

-export([file/1, file/2]).
-export([string/1, string/2]).
-export([tokens/1]).

file(Filename) ->
    file(Filename, #{}).
file(Filename, Opts) ->
    case file:read_file(Filename) of
	{ok,Binary} ->
	    string(Filename, Binary, Opts);
	Error ->
	    Error
    end.

string(String) ->
    string("*internal*", String).

string(File, String) ->
    string(File, String, #{}).

string(File, Binary, Opts) when is_binary(Binary) ->
    string(File, binary_to_list(Binary), Opts);    
string(File, String, _Opts) when is_list(String) ->
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
