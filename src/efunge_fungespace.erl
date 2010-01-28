%%%----------------------------------------------------------------------
%%% efunge - a Befunge-98 interpreter in Erlang.
%%% Copyright (C) 2008-2009 Arvid Norlander <anmaster AT tele2 DOT se>
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
%% The current implementation also uses the process dictionary key fspacebounds
%% to store the bounds of the Funge-Space. This may change without prior notice.
-module(efunge_fungespace).
-export([create/1, set/3, set/4, load/5, save/6,
         fetch/2, fetch/3,
         delete/1, get_bounds/1, get_bounds_exact/1]).
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
set(Fungespace, {_X,_Y} = Coord, $\s) ->
	ets:delete(Fungespace, Coord),
	update_bounds($\s, Coord);
set(Fungespace, {_X,_Y} = Coord, V) ->
	ets:insert(Fungespace, {Coord, V}),
	update_bounds(V, Coord).

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
		[{_,Value}] -> Value
	end.

%% @spec create(Filename::string()) -> fungespace()
%% @doc Create a Funge Space from a file.
-spec create(string()) -> fungespace().
create(Filename) ->
	{ok, Binary} = file:read_file(Filename),
	Fungespace = construct(),
	load_binary(Binary, Fungespace, 0, 0, false, 0, undefined),
	Fungespace.

%% @spec load(fungespace(), ip(), Filename::string(), IsBinaryMode::boolean(), coord()) -> error | coord()
%% @doc Loads a file into an existing Funge Space, returning the max size.
-spec load(fungespace(), ip(), string(), boolean(), coord()) -> error | coord().
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

-spec save(fungespace(), ip(), string(), boolean(), coord(), coord()) -> error | ok.
save(_Fungespace, #fip{}, "", _IsTextFile, {_X, _Y}, {_W, _H}) ->
	error;
save(_Fungespace, #fip{}, _Filename, _IsTextFile, {_X, _Y}, {W, H}) when W =< 0; H =< 0 ->
	error;
save(Fungespace, #fip{offX = OffX, offY = OffY}, Filename, IsTextFile, {X, Y}, {W,H}) ->
	TrueX = OffX+X,
	TrueY = OffY+Y,
	Bin =
		case IsTextFile of
			true -> save_text(Fungespace, TrueX, TrueY, TrueX+W, TrueY+H);
			false -> save_binary(Fungespace, TrueX, TrueY, TrueX+W, TrueY+H)
		end,
	case file:write_file(Filename, Bin) of
		{error, _} -> error;
		ok -> ok
	end.

%% @spec delete(fungespace()) -> true
%% @private For use in core on exit only.
%% @doc Destroy a Funge Space.
-spec delete(fungespace()) -> true.
delete(Fungespace) ->
	ets:delete(Fungespace).

%% @spec get_bounds(fungespace()) -> {LeastPoint::coord(), GreatestPoint::coord()}
%% @doc Get Funge Space bounds.
-spec get_bounds(fungespace()) -> rect().
get_bounds(_Fungespace) ->
	get(fspacebounds).


%% @spec get_bounds_exact(fungespace()) -> {LeastPoint::coord(), GreatestPoint::coord()}
%% @doc Get exact Funge Space bounds.
-spec get_bounds_exact(fungespace()) -> rect().
get_bounds_exact(Fungespace) ->
	case get(fspacebounds_exact) of
		true ->
			get(fspacebounds);
		false ->
			recalculate_bounds_exact(Fungespace)
	end.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @doc Construct a Funge Space.
-spec construct() -> fungespace().
construct() ->
	Space = ets:new(fungespace, [set, private]),
	put(fspacebounds, {{undefined, undefined}, {undefined, undefined}}),
	put(fspacebounds_exact, true),
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
-spec update_bounds(integer(), coord()) -> 'true'.
update_bounds($\s, {X,Y}) ->
	case get(fspacebounds_exact) of
		false -> true;
		true  ->
			{{MinX,MinY},{MaxX,MaxY}} = get(fspacebounds),
			if
				X =:= MinX; X =:= MaxX; Y =:= MinY; Y =:= MaxY ->
					put(fspacebounds_exact, false),
					true;
				true ->
					true
			end
	end;
update_bounds(_V, {X,Y}) ->
	{{MinX,MinY},{MaxX,MaxY}} = get(fspacebounds),
	MinX1 = find_bounds_min(MinX, X),
	MinY1 = find_bounds_min(MinY, Y),
	MaxX1 = find_bounds_max(MaxX, X),
	MaxY1 = find_bounds_max(MaxY, Y),
	put(fspacebounds, {{MinX1,MinY1},{MaxX1,MaxY1}}),
	true.

%% @doc Find the extreme values in the list.
-spec find_extremes([{cell(),cell()}],cell(),cell(),cell(),cell()) -> rect().
find_extremes([], MinX, MinY, MaxX, MaxY) ->
	{{MinX,MinY},{MaxX,MaxY}};
find_extremes([{X,Y}|T], MinX, MinY, MaxX, MaxY) ->
	find_extremes(T, erlang:min(MinX,X),erlang:min(MinY,Y),
	                 erlang:max(MaxX,X),erlang:max(MaxY,Y)).

-spec recalculate_bounds_exact(fungespace()) -> rect().
recalculate_bounds_exact(Fungespace) ->
	% Get first item as base for new bounds.
	[{FirstX,FirstY}|Coordinates] = ets:select(Fungespace, [{{'$1','$2'},[{'=/=','$2',$\s}],['$1']}]),
	NewBounds = find_extremes(Coordinates, FirstX, FirstY, FirstX, FirstY),
	put(fspacebounds, NewBounds),
	put(fspacebounds_exact, true),
	NewBounds.

%% @doc Dump funge space to a binary (binary mode to o)
-spec save_binary(fungespace(),cell(),cell(),cell(),cell()) -> binary().
save_binary(Fungespace, MinX, MinY, MaxX, MaxY) ->
	save_binary(<<>>, MinX, MinY, Fungespace, MinX, MaxX, MaxY).

%% @doc Binary mode o, "main loop"
-spec save_binary(binary(),cell(),cell(),fungespace(),cell(),cell(),cell()) -> binary().
save_binary(Bin, _CurX, MaxY, _Fungespace, _MinX, _MaxX, MaxY) ->
	Bin;
save_binary(Bin, MaxX, CurY, Fungespace, MinX, MaxX, MaxY) ->
	save_binary(<<Bin/binary, $\n>>, MinX, CurY+1, Fungespace, MinX, MaxX, MaxY);
save_binary(Bin, CurX, CurY, Fungespace, MinX, MaxX, MaxY) ->
	Value = fetch(Fungespace, {CurX, CurY}),
	save_binary(<<Bin/binary, (Value rem 256)/integer>>, CurX+1, CurY, Fungespace, MinX, MaxX, MaxY).

%% @doc Dump funge space to a binary (text mode to o)
-spec save_text(fungespace(),cell(),cell(),cell(),cell()) -> binary().
save_text(Fungespace, MinX, MinY, MaxX, MaxY) ->
	save_text([], [], MinX, MinY, Fungespace, MinX, MaxX, MaxY).

%% @doc Helper for text mode o: Strips trailing whitespaces.
-spec fixup_text_line([integer()]) -> [integer(),...].
fixup_text_line([]) ->
	[$\n];
fixup_text_line([$\s|T]) ->
	fixup_text_line(T);
fixup_text_line([_|_] = Str) ->
	lists:reverse([$\n|Str]).

%% @doc
%% Helper for text mode o: Strips trailing newlines.
%% Horribly inefficient in part.
-spec fixup_text_final([[integer(),...]]) -> [[integer()]].
fixup_text_final([]) ->
	[];
fixup_text_final([[$\n]|T]) ->
	fixup_text_final(T);
fixup_text_final([H|T]) ->
	% Remove last newline from here.
	% We have a logic error if there is none.
	[$\n|Str] = lists:reverse(H),
	lists:reverse([lists:reverse(Str)|T]).

%% @doc Text mode o, "main" loop.
-spec save_text([[integer(),...]],[integer()],cell(),cell(),fungespace(),cell(),cell(),cell()) -> binary().
save_text(String, _CurLn, _CurX, MaxY, _Fungespace, _MinX, _MaxX, MaxY) ->
	list_to_binary(fixup_text_final(String));
save_text(String, CurLn, MaxX, CurY, Fungespace, MinX, MaxX, MaxY) ->
	save_text([fixup_text_line(CurLn)|String], [], MinX, CurY+1, Fungespace, MinX, MaxX, MaxY);
save_text(String, CurLn, CurX, CurY, Fungespace, MinX, MaxX, MaxY) ->
	Value = fetch(Fungespace, {CurX, CurY}),
	save_text(String, [Value rem 256|CurLn], CurX+1, CurY, Fungespace, MinX, MaxX, MaxY).

%% @spec load_binary(Binary, fungespace(), X, Y, LastWasCR, MinX, MaxX) -> coord()
%% @doc
%% Load a binary into Funge Space. MinX is used for knowing what least X
%% should be used when resetting due to newline, and not loading from 0,0
%% MaxX is used for making return value work.
-spec load_binary(binary(),fungespace(),integer(),integer(),boolean(),integer(),integer_or_undef()) -> coord().
load_binary(<<H,T/binary>>, FungeSpace, X, Y, LastWasCR, MinX, MaxX) ->
	case H of
		$\n ->
			case LastWasCR of
				true -> load_binary(T, FungeSpace, MinX, Y, false, MinX, MaxX);
				false -> load_binary(T, FungeSpace, MinX, Y+1, false, MinX, find_bounds_max(MaxX, X))
			end;
		%% Form feed is ignored.
		$\f ->
			load_binary(T, FungeSpace, X, Y, false, MinX, MaxX);
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
