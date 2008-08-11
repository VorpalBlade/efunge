-module(fspace).
%% Handle Funge Space.
-export([load/1, set/3, fetch/2, dump/1]).

%% External:
%%  set(Dict, {X, Y}, Value) -> dict
%%  fetch(Dict, {X, Y}) -> value
%%  dump(Dict) -> noreply
%%    Side effect: print out funge space for debugging.
%%  load(Filename) -> dict
%% Internal:
%%  loadChars(Dict, Y, X, Line) -> filled in line
%%  loadLines(File, Dict, Y) -> filled dict
%%  for(I, Max, F) -> noreply.
%%    A for loop.


%% Public functions

set(D, {_,_} = Coord, V) ->
	dict:store(Coord, V, D).


%% Use dict:find/2?
fetch(D, {_,_} = Coord) ->
	case dict:is_key(Coord, D) of
		true  -> dict:fetch(Coord, D);
		false -> $\s
	end.


load(Filename) when is_list(Filename) ->
	{ok, File} = file:open(Filename, read),
	loadLines(File, dict:new(), 0).

dump(Dict) ->
	F = fun(Y) -> dumpLine(Dict, Y, 0), io:format("~n", []) end,
	for(0, 25, F),
	noreply.



%% Private functions
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

for(Max, Max, F) ->
	F(Max);
for(I, Max, F)   ->
	F(I),
	for(I+1, Max, F).
