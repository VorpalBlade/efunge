-module(fspace).
%% Handle Funge Space.
%% Format of tuples in table is {{X,Y},Value}
-export([load/1, set/3, fetch/2, delete/1]).

%% Public functions

%% set(Table, Coord::tuple(), V::int()) -> Table.
set(Table, {_,_} = Coord, V) ->
	ets:insert(Table, {Coord, V}).


%% fetch(Table, Coord::tuple()) -> int().
fetch(Table, {_,_} = Coord) ->
	Result = ets:lookup(Table, Coord),
	case Result of
		[] -> $\s;
		[{{_,_},Value}] -> Value
	end.

%% load(Filename::string()) -> dictionary().
load(Filename) ->
	{ok, File} = file:open(Filename, [read]),
	D = ets:new(fungespace, [set, private]),
	loadLines(File, D, 0),
	file:close(File),
	D.

%% delete(Table) -> true.
delete(Table) ->
	ets:delete(Table).


%% Private functions

%% loadChars(Dict::dictionary(), Y:int(), X:int(), string()) -> true.
%%   Load everything from one line.
loadChars(_, _, _, []) ->
	true;
loadChars(Dict, Y, X, [H|T]) ->
	if
		(H =:= $\n) orelse (H =:= $\r) ->
			%% May contain ending newlines...
			true;
		true ->
			set(Dict, {X, Y}, H),
			loadChars(Dict, Y, X+1, T)
	end.

%% loadLines(File, Dict:dictionary(), Y:int()) -> true.
%%   Load a line at the the time, then tail recursive call to load the next one.
loadLines(_, _, 26) ->
	true;
loadLines(File, Dict, Y) ->
	Line = io:get_line(File, ''),
	if
		(Line =:= eof) orelse (Y > 25) ->
			true;
		true ->
			loadChars(Dict, Y, 0, Line),
			loadLines(File, Dict, Y+1)
	end.
