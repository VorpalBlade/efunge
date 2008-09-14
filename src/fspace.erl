%% @doc Handle Funge Space.
%%
%% Format of tuples in table is {{X,Y},Value}. The current implementation use
%% ETS tables, but that may change without prior notice.
-module(fspace).
-include("fip.hrl").
-include("fspace.hrl").
-include("funge_types.hrl").
-export([load/1, set/3, fetch/2, delete/1, getBounds/1]).

%% Public functions

%% @type coord() = {X::integer(), Y::integer()}.
%%   Funge Space coordinates.
%% @type fungespace() = #fspace{}.
%%   A Funge Space.

%% @spec set(fungespace(), coord(), V::integer()) -> true
%% @doc Set a cell in Funge Space.
-spec set(fungespace(), coord(), integer()) -> true.
set(#fspace{} = Fungespace, {_X,_Y} = Coord, V) ->
	Table = Fungespace#fspace.space,
	ets:insert(Table, {Coord, V}),
	updateBounds(Table, Coord).


%% @spec fetch(fungespace(), coord()) -> integer()
%% @doc Get a cell from a specific Funge Space.
-spec fetch(fungespace(), coord()) -> integer().
fetch(#fspace{} = Fungespace, {_X,_Y} = Coord) ->
	Result = ets:lookup(Fungespace#fspace.space, Coord),
	case Result of
		[] -> $\s;
		[{{_,_},Value}] -> Value
	end.


%% @spec load(Filename::string()) -> fungespace()
%% @doc Create a Funge Space from a file.
-spec load(string()) -> fungespace().
load(Filename) ->
	{ok, File} = file:open(Filename, [read]),
	FungeSpace = #fspace{ space = create()},
	loadLines(File, FungeSpace, 0),
	file:close(File),
	FungeSpace.

%% @spec delete(fungespace()) -> true
%% @doc Destroy a Funge Space.
-spec delete(fungespace()) -> true.
delete(#fspace{} = Fungespace) ->
	ets:delete(Fungespace#fspace.space).

%% @spec getBounds(fungespace()) -> {LeastPoint::coord(), GreatestPoint::coord()}.
%% @doc Get Funge Space bounds.
-spec getBounds(fungespace()) -> {coord(), coord()}.
getBounds(#fspace{} = Fungespace) ->
	Space = Fungespace#fspace.space,
	[{_,MinX}] = ets:lookup(Space, minx),
	[{_,MinY}] = ets:lookup(Space, miny),
	[{_,MaxX}] = ets:lookup(Space, maxx),
	[{_,MaxY}] = ets:lookup(Space, maxy),
	{{MinX, MinY}, {MaxX, MaxY}}.

%% Private functions

%% @doc Create a Funge Space.
create() ->
	Space = ets:new(fungespace, [set, private]),
	ets:insert(Space, {minx, undefined}),
	ets:insert(Space, {miny, undefined}),
	ets:insert(Space, {maxx, undefined}),
	ets:insert(Space, {maxy, undefined}),
	Space.


%% @doc Finds minimum.
-spec boundsMin(undefined | integer(), integer()) -> integer().
boundsMin(undefined, Y)    -> Y;
boundsMin(X, Y) when X < Y -> X;
boundsMin(_X, Y) -> Y.

%% @doc Finds maximum.
-spec boundsMax(undefined | integer(), integer()) -> integer().
boundsMax(undefined, Y)    -> Y;
boundsMax(X, Y) when X > Y -> X;
boundsMax(_X, Y) -> Y.

%% @doc Update bounds values in tables.
-spec updateBounds(fungespace_table(), coord()) -> 'true'.
updateBounds(Space, {X,Y}) ->
	[{_,MinX}] = ets:lookup(Space, minx),
	[{_,MinY}] = ets:lookup(Space, miny),
	[{_,MaxX}] = ets:lookup(Space, maxx),
	[{_,MaxY}] = ets:lookup(Space, maxy),
	MinX1 = boundsMin(MinX, X),
	MinY1 = boundsMin(MinY, Y),
	MaxX1 = boundsMax(MaxX, X),
	MaxY1 = boundsMax(MaxY, Y),
	ets:insert(Space, {minx, MinX1}),
	ets:insert(Space, {miny, MinY1}),
	ets:insert(Space, {maxx, MaxX1}),
	ets:insert(Space, {maxy, MaxY1}),
	true.

%% @spec loadChars(fungespace(), Y::integer(), X::integer(), string()) -> true
%% @doc Load everything from one line.
loadChars(_, _, _, []) ->
	true;
loadChars(FungeSpace, Y, X, [H|T]) ->
	if
		(H =:= $\n) orelse (H =:= $\r) ->
			%% May contain ending newlines...
			true;
		true ->
			set(FungeSpace, {X, Y}, H),
			loadChars(FungeSpace, Y, X+1, T)
	end.

%% @spec loadLines(File, fungespace(), Y::integer()) -> true
%% @doc Load a line at the the time, then tail recursive call to load the next one.
loadLines(File, FungeSpace, Y) ->
	Line = io:get_line(File, ''),
	if
		(Line =:= eof) ->
			true;
		true ->
			loadChars(FungeSpace, Y, 0, Line),
			loadLines(File, FungeSpace, Y+1)
	end.
