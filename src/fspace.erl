%%%----------------------------------------------------------------------
%%% efunge - a Befunge-98 interpreter in Erlang.
%%% Copyright (C) 2008 Arvid Norlander <anmaster AT tele2 DOT se>
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%----------------------------------------------------------------------
%% @doc Handle Funge Space.
%%
%% Format of tuples in table is {{X,Y},Value}. The current implementation use
%% ETS tables, but that may change without prior notice.
%%
%% The current implementation also use some special keys to store metadata like
%% bounds of the Funge-Space.
-module(fspace).
-include("fip.hrl").
-include("funge_types.hrl").
-export([load/1, set/3, set/4, fetch/2, fetch/3, delete/1, get_bounds/1]).

%% Public functions

%% @type coord() = {X::integer(), Y::integer()}.
%%   Funge Space coordinates.
%% @type fungespace() = integer().
%%   A Funge Space.

%% @spec set(fungespace(), ip(), coord(), V::integer()) -> true
%% @doc Set a cell in Funge Space with storage offset taken from IP.
-spec set(fungespace(), ip(), coord(), integer()) -> true.
set(Fungespace, #fip{offX = OffX, offY = OffY}, {X,Y}, V) ->
	set(Fungespace, {X+OffX, Y+OffY}, V).

%% @spec set(fungespace(), coord(), V::integer()) -> true
%% @doc Set a cell in Funge Space.
-spec set(fungespace(), coord(), integer()) -> true.
set(Fungespace, {_X,_Y} = Coord, V) ->
	ets:insert(Fungespace, {Coord, V}),
	update_bounds(V, Fungespace, Coord).

%% @spec fetch(fungespace(), ip(), coord()) -> integer()
%% @doc Get a cell from a specific Funge Space.
-spec fetch(fungespace(), ip(), coord()) -> integer().
fetch(Fungespace, #fip{offX = OffX, offY = OffY}, {X,Y}) ->
	fetch(Fungespace, {X+OffX, Y+OffY}).

%% @spec fetch(fungespace(), coord()) -> integer()
%% @doc Get a cell from a specific Funge Space.
-spec fetch(fungespace(), coord()) -> integer().
fetch(Fungespace, {_X,_Y} = Coord) ->
	case ets:lookup(Fungespace, Coord) of
		[] -> $\s;
		[{{_,_},Value}] -> Value
	end.


%% @spec load(Filename::string()) -> fungespace()
%% @doc Create a Funge Space from a file.
-spec load(string()) -> fungespace().
load(Filename) ->
	{ok, Binary} = file:read_file(Filename),
	FungeSpace = create(),
	load_binary(Binary, FungeSpace, 0, 0, false),
	FungeSpace.

%% @spec delete(fungespace()) -> true
%% @doc Destroy a Funge Space.
-spec delete(fungespace()) -> true.
delete(Fungespace) ->
	ets:delete(Fungespace).

%% @spec get_bounds(fungespace()) -> {LeastPoint::coord(), GreatestPoint::coord()}
%% @doc Get Funge Space bounds.
-spec get_bounds(fungespace()) -> {coord(), coord()}.
get_bounds(Fungespace) ->
	[{_,MinX}] = ets:lookup(Fungespace, minx),
	[{_,MinY}] = ets:lookup(Fungespace, miny),
	[{_,MaxX}] = ets:lookup(Fungespace, maxx),
	[{_,MaxY}] = ets:lookup(Fungespace, maxy),
	{{MinX, MinY}, {MaxX, MaxY}}.

%% Private functions

%% @doc Create a Funge Space.
-spec create() -> fungespace().
create() ->
	Space = ets:new(fungespace, [set, private]),
	ets:insert(Space, {minx, undefined}),
	ets:insert(Space, {miny, undefined}),
	ets:insert(Space, {maxx, undefined}),
	ets:insert(Space, {maxy, undefined}),
	Space.


%% @doc Finds minimum.
-spec find_bounds_min(undefined | integer(), integer()) -> integer().
find_bounds_min(undefined, Y)    -> Y;
find_bounds_min(X, Y) when X < Y -> X;
find_bounds_min(_X, Y) -> Y.

%% @doc Finds maximum.
-spec find_bounds_max(undefined | integer(), integer()) -> integer().
find_bounds_max(undefined, Y)    -> Y;
find_bounds_max(X, Y) when X > Y -> X;
find_bounds_max(_X, Y) -> Y.

%% @doc Update bounds values in tables.
-spec update_bounds(integer(), fungespace(), coord()) -> 'true'.
update_bounds($\s, _Space, _Coord) ->
	true;
update_bounds(_V, Space, {X,Y}) ->
	[{_,MinX}] = ets:lookup(Space, minx),
	[{_,MinY}] = ets:lookup(Space, miny),
	[{_,MaxX}] = ets:lookup(Space, maxx),
	[{_,MaxY}] = ets:lookup(Space, maxy),
	MinX1 = find_bounds_min(MinX, X),
	MinY1 = find_bounds_min(MinY, Y),
	MaxX1 = find_bounds_max(MaxX, X),
	MaxY1 = find_bounds_max(MaxY, Y),
	ets:insert(Space, {minx, MinX1}),
	ets:insert(Space, {miny, MinY1}),
	ets:insert(Space, {maxx, MaxX1}),
	ets:insert(Space, {maxy, MaxY1}),
	true.

%% @spec load_lines(Binary, fungespace(), X, Y, LastWasCR) -> true
%% @doc Load a binary into Funge Space.
-spec load_binary(binary(),fungespace(),non_neg_integer(),non_neg_integer(),bool()) -> true.
load_binary(<<H,T/binary>>, FungeSpace, X, Y, LastWasCR) ->
	case H of
		$\n ->
			case LastWasCR of
				true -> load_binary(T, FungeSpace, 0, Y, false);
				false -> load_binary(T, FungeSpace, 0, Y+1, false)
			end;
		$\r ->
			load_binary(T, FungeSpace, 0, Y+1, true);
		%% Spaces shouldn't replace.
		$\s ->
			load_binary(T, FungeSpace, X+1, Y, false);
		_ ->
			set(FungeSpace, {X, Y}, H),
			load_binary(T, FungeSpace, X+1, Y, false)
	end;
load_binary(<<>>, _FungeSpace, _X, _Y, _LastWasCR) -> true.
