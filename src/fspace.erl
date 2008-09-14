%% @doc Handle Funge Space.
%%
%% Format of tuples in table is {{X,Y},Value}. The current implementation use
%% ETS tables, but that may change without prior notice.
-module(fspace).
-include("fip.hrl").
-include("fspace.hrl").
-include("funge_types.hrl").
-export([load/1, set/3, fetch/2, delete/1]).

%% Public functions

%% @type coord() = {X::integer(), Y::integer()}.
%%   Funge Space coordinates.
%% @type fungespace() = tid().
%%   A Funge Space.

%% @spec set(fungespace(), coord(), V::integer()) -> true
%% @doc Set a cell in Funge Space.
-spec set(fungespace(), coord(), integer()) -> true.
set(Table, {_X,_Y} = Coord, V) ->
	ets:insert(Table, {Coord, V}).


%% @spec fetch(fungespace(), coord()) -> integer()
%% @doc Get a cell from a specific Funge Space.
-spec fetch(fungespace(), coord()) -> fungespace().
fetch(Table, {_X,_Y} = Coord) ->
	Result = ets:lookup(Table, Coord),
	case Result of
		[] -> $\s;
		[{{_,_},Value}] -> Value
	end.


%% @spec load(Filename::string()) -> tid()
%% @doc Create a Funge Space from a file.
-spec load(string()) -> integer().
load(Filename) ->
	{ok, File} = file:open(Filename, [read]),
	Tab = ets:new(fungespace, [set, private]),
	loadLines(File, Tab, 0),
	file:close(File),
	Tab.

%% @spec delete(fungespace()) -> true
%% @doc Destroy a Funge Space.
-spec delete(fungespace()) -> true.
delete(Table) ->
	ets:delete(Table).


%% Private functions

%% @spec loadChars(fungespace(), Y::integer(), X::integer(), string()) -> true
%% @doc Load everything from one line.
loadChars(_, _, _, []) ->
	true;
loadChars(Table, Y, X, [H|T]) ->
	if
		(H =:= $\n) orelse (H =:= $\r) ->
			%% May contain ending newlines...
			true;
		true ->
			set(Table, {X, Y}, H),
			loadChars(Table, Y, X+1, T)
	end.

%% @spec loadLines(File, fungespace(), Y::integer()) -> true
%% @doc Load a line at the the time, then tail recursive call to load the next one.
loadLines(_, _, 26) ->
	true;
loadLines(File, Table, Y) ->
	Line = io:get_line(File, ''),
	if
		(Line =:= eof) orelse (Y > 25) ->
			true;
		true ->
			loadChars(Table, Y, 0, Line),
			loadLines(File, Table, Y+1)
	end.
