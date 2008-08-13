-module(fspace).
%% Handle Funge Space.
-export([load/1, set/3, fetch/2, dump/1]).

%% Public functions

%% set(D::dictionary(), Coord::tuple(), V::int()) -> dictionary().
set(D, {_,_} = Coord, V) ->
	dict:store(Coord, V, D).


%% fetch(D::dictionary(), Coord::tuple()) -> int().
fetch(D, {_,_} = Coord) ->
	%% Use dict:find/2?
	case dict:is_key(Coord, D) of
		true  -> dict:fetch(Coord, D);
		false -> $\s
	end.

%% load(Filename::string())-> dictionary().
load(Filename) ->
	{ok, File} = file:open(Filename, [read]),
	D = loadLines(File, dict:new(), 0),
	file:close(File),
	D.

%% dump(Dict::dictionary())-> noreply.
%%   Side effect: print out funge space for debugging.
dump(Dict) ->
	F = fun(Y) -> dumpLine(Dict, Y, 0), io:format("~n", []) end,
	for(0, 25, F),
	noreply.



%% Private functions

%% loadChars(Dict::dictionary(), Y:int(), X:int(), string())-> NewDict::dictionary().
%%   Load everything from one line.
loadChars(Dict, _, _, []) ->
	Dict;
loadChars(Dict, Y, X, [H|T]) ->
	if
		(H =:= $\n) orelse (H =:= $\r) ->
			%% May contain ending newlines...
			Dict;
		true ->
			NewDict = set(Dict, {X, Y}, H),
			loadChars(NewDict, Y, X+1, T)
	end.

%% loadLines(File, Dict:dictionary(), Y:int())-> NewDict::dictionary().
%%   Load a line at the the time, then tail recursive call to load the next one.
loadLines(_, Dict, 26) ->
	Dict;
loadLines(File, Dict, Y) ->
	Line = io:get_line(File, ''),
	if
		(Line =:= eof) orelse (Y > 25) ->
			Dict;
		true ->
			NewDict = loadChars(Dict, Y, 0, Line),
			loadLines(File, NewDict, Y+1)
	end.


dumpLine(_, _, 81) ->
	noreply;
dumpLine(Dict, Y, X) ->
	io:format("~c", [fetch(Dict, {X,Y})]),
	dumpLine(Dict, Y, X+1).

%% These are based on examples in Programming Erlang.
for(Max, Max, F) ->
	F(Max);
for(I, Max, F)   ->
	F(I),
	for(I+1, Max, F).
