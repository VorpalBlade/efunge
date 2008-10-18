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
%% @doc This module handles the Funge Space.
%%
%% Format of tuples in table is {{X,Y},Value}. The current implementation use
%% ETS tables, but that may change without prior notice.
%%
%% The current implementation also use some special keys to store metadata like
%% bounds of the Funge-Space.
-module(efunge_fungespace).
-export([create/1, set/3, set/4, load/5,
         fetch/2, fetch/3,
         delete/1, get_bounds/1]).
-include("efunge_ip.hrl").
-include("funge_types.hrl").
%% @headerfile "efunge_ip.hrl"

%% Public functions

%% @type coord() = {X::integer(), Y::integer()}.
%%   Funge Space coordinates.
%% @type fungespace().
%%   A Funge Space. The actual type is internal.

-type integer_or_undef() :: undefined | integer().

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
%% @doc Get a cell from a specific Funge Space. Will use storage offset of IP.
-spec fetch(fungespace(), ip(), coord()) -> cell().
fetch(Fungespace, #fip{offX = OffX, offY = OffY}, {X,Y}) ->
	fetch(Fungespace, {X+OffX, Y+OffY}).

%% @spec fetch(fungespace(), coord()) -> integer()
%% @doc Get a cell from a specific Funge Space.
-spec fetch(fungespace(), coord()) -> cell().
fetch(Fungespace, {_X,_Y} = Coord) ->
	case ets:lookup(Fungespace, Coord) of
		[] -> $\s;
		[{{_,_},Value}] -> Value
	end.

%% @spec create(Filename::string()) -> fungespace()
%% @doc Create a Funge Space from a file.
-spec create(string()) -> fungespace().
create(Filename) ->
	{ok, Binary} = file:read_file(Filename),
	Fungespace = construct(),
	load_binary(Binary, Fungespace, 0, 0, false, 0, undefined),
	Fungespace.

%% @spec load(fungespace(), ip(), Filename::string(), IsBinaryMode::bool(), coord()) -> error | coord()
%% @doc Loads a file into an existing Funge Space, returning the max size.
-spec load(fungespace(), ip(), string(), bool(), coord()) -> error | coord().
load(Fungespace, #fip{offX = OffX, offY = OffY}, Filename, IsBinaryMode, {X, Y} = _Coord) ->
	{Status, Binary} = file:read_file(Filename),
	% TODO: Make binary mode work.
	case Status of
		error ->
			error;
		ok ->
			TrueX = OffX + X,
			TrueY = OffY + Y,
			case IsBinaryMode of
				false ->
					{MaxX, MaxY} = load_binary(Binary, Fungespace, TrueX, TrueY, false, TrueX, undefined);
				true ->
					MaxX = load_binary_no_newlines(Binary, Fungespace, TrueX, TrueY),
					MaxY = TrueY
			end,
			{MaxX - TrueX, MaxY - TrueY}
	end.



%% @spec delete(fungespace()) -> true
%% @private For use in core on exit only.
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

%% @doc Construct a Funge Space.
-spec construct() -> fungespace().
construct() ->
	Space = ets:new(fungespace, [set, private]),
	ets:insert(Space, {minx, undefined}),
	ets:insert(Space, {miny, undefined}),
	ets:insert(Space, {maxx, undefined}),
	ets:insert(Space, {maxy, undefined}),
	Space.


%% @doc Finds minimum.
-spec find_bounds_min(integer_or_undef(), integer()) -> integer().
find_bounds_min(undefined, Y)    -> Y;
find_bounds_min(X, Y) when X < Y -> X;
find_bounds_min(_X, Y)           -> Y.

%% @doc Finds maximum.
-spec find_bounds_max(integer_or_undef(), integer()) -> integer().
find_bounds_max(undefined, Y)    -> Y;
find_bounds_max(X, Y) when X > Y -> X;
find_bounds_max(_X, Y)           -> Y.

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

%% @spec load_binary(Binary, fungespace(), X, Y, LastWasCR, MinX, MaxX) -> coord()
%% @doc
%% Load a binary into Funge Space. MinX is used for knowing what least X
%% should be used when resetting due to newline, and not loading from 0,0
%% MaxX is used for making return value work.
-spec load_binary(binary(),fungespace(),integer(),integer(),bool(),integer(),integer_or_undef()) -> coord().
load_binary(<<H,T/binary>>, FungeSpace, X, Y, LastWasCR, MinX, MaxX) ->
	case H of
		$\n ->
			case LastWasCR of
				true -> load_binary(T, FungeSpace, MinX, Y, false, MinX, MaxX);
				false -> load_binary(T, FungeSpace, MinX, Y+1, false, MinX, find_bounds_max(MaxX, X))
			end;
		$\r ->
			load_binary(T, FungeSpace, MinX, Y+1, true, MinX, find_bounds_max(MaxX, X));
		%% Spaces shouldn't replace.
		$\s ->
			load_binary(T, FungeSpace, X+1, Y, false, MinX, MaxX);
		_ ->
			set(FungeSpace, {X, Y}, H),
			load_binary(T, FungeSpace, X+1, Y, false, MinX, MaxX)
	end;
load_binary(<<>>, _FungeSpace, X, Y, _LastWasCR, _MinX, MaxX) ->
	{find_bounds_max(MaxX, X), Y}.

%% @spec load_binary_no_newlines(Binary, fungespace(), X, Y) -> MaxX
%% @doc Load everything in the binary, without going to a new row on newline.
-spec load_binary_no_newlines(binary(),fungespace(),integer(),integer()) -> integer().
load_binary_no_newlines(<<H,T/binary>>, FungeSpace, X, Y) ->
	set(FungeSpace, {X, Y}, H),
	load_binary_no_newlines(T, FungeSpace, X+1, Y);
load_binary_no_newlines(<<>>, _FungeSpace, X, _Y) ->
	X.
