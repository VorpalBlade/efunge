%%%----------------------------------------------------------------------
%%% efunge - a Befunge-98 interpreter in Erlang.
%%% Copyright (C) 2008-2008 Arvid Norlander <anmaster AT tele2 DOT se>
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
%% @doc This module implements manipulation functions for the IP.
-module(efunge_ip).
-export([ip_forward/2, jump/3]).
-export([set_delta/3, rev_delta/1, set_offset/3]).
-export([turn_delta_left/1, turn_delta_right/1]).
-export([find_next_match/3, find_next_non_space/2]).
-include("efunge_ip.hrl").
-include("funge_types.hrl").
%% @headerfile "efunge_ip.hrl"


%%====================================================================
%% API
%%====================================================================

%% @spec ip_forward(ip(), fungespace()) -> NewIP::ip()
%% @doc Move IP forward one step.
-spec ip_forward(ip(), fungespace()) -> ip().
ip_forward(#fip{x=X, y=Y, dx=DX, dy=DY} = IP, FungeSpace) ->
	Bounds = efunge_fungespace:get_bounds(FungeSpace),
	NewX = X+DX,
	NewY = Y+DY,
	NewIP = IP#fip{ x=NewX, y=NewY },
	case is_in_range({NewX, NewY}, Bounds) of
		true -> NewIP;
		false ->
			case is_delta_cardinal(IP) of
				true -> ip_forward_cardinal(NewIP, Bounds);
				false -> ip_forward_flying(rev_delta(IP), Bounds)
			end
	end.

%% @doc Handles j correctly, it will temporarly change delta for the IP then
%% jump forward, and finally restore delta.
-spec jump(ip(), fungespace(), integer()) -> ip().
jump(#fip{dx=DX, dy=DY} = IP, FungeSpace, Distance) ->
	IPNewDelta = IP#fip{ dx = DX * Distance, dy = DY * Distance},
	IPNewPos = ip_forward(IPNewDelta, FungeSpace),
	IPNewPos#fip{ dx = DX, dy = DY }.

%% @spec set_delta(ip(), X::integer(), Y::integer()) -> NewState::ip()
%% @doc Set delta in state.
-spec set_delta(ip(), integer(), integer()) -> ip().
set_delta(#fip{} = IP, X, Y) ->
	IP#fip{ dx = X, dy = Y }.

%% @spec rev_delta(ip()) -> NewState::ip()
%% @doc Reverse IP.
-spec rev_delta(ip()) -> ip().
rev_delta(#fip{dx=DX, dy=DY} = IP) ->
	IP#fip{ dx = -DX, dy = -DY }.

%% @spec set_offset(ip(), X::integer(), Y::integer()) -> NewState::ip()
%% @doc Set delta in state.
-spec set_offset(ip(), integer(), integer()) -> ip().
set_offset(#fip{} = IP, X, Y) ->
	IP#fip{ offX = X, offY = Y }.

%% @doc Turn IP left.
-spec turn_delta_left(ip()) -> ip().
turn_delta_left(#fip{dx=DX, dy=DY} = IP) ->
	IP#fip{ dx = DY, dy = -DX }.

%% @doc Turn IP right.
-spec turn_delta_right(ip()) -> ip().
turn_delta_right(#fip{dx=DX, dy=DY} = IP) ->
	IP#fip{ dx = -DY, dy = DX }.

%% @doc Search in IP's path for the next time value shows up.
-spec find_next_match(ip(), integer(), fungespace()) -> ip().
find_next_match(#fip{x=X, y=Y} = IP, Match, FungeSpace) ->
	Value = efunge_fungespace:fetch(FungeSpace, {X, Y}),
	if
		Value =:= Match -> IP#fip{x = X, y = Y};
		true -> find_next_match(ip_forward(IP, FungeSpace), Match, FungeSpace)
	end.

%% @doc Find next one that isn't a whitespace, and isn't in ;;.
-spec find_next_non_space(ip(), fungespace()) -> {ip(), integer()}.
find_next_non_space(#fip{x=X, y=Y} = IP, FungeSpace) ->
	Value = efunge_fungespace:fetch(FungeSpace, {X, Y}),
	if
		Value =:= $\s ->
			find_next_non_space(ip_forward(IP, FungeSpace), FungeSpace);
		Value =:= $; ->
			IP2 = find_next_match(ip_forward(IP, FungeSpace), $;, FungeSpace),
			find_next_non_space(ip_forward(IP2, FungeSpace), FungeSpace);
		true ->
			{IP#fip{x = X, y = Y}, Value}
	end.


%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Check if IP is cardinal
-spec is_delta_cardinal(ip()) -> bool().
is_delta_cardinal(#fip{dx=DX, dy=DY}) ->
	case {DX, DY} of
		{ 0,  1} -> true;
		{ 0, -1} -> true;
		{ 1,  0} -> true;
		{-1,  0} -> true;
		_ -> false
	end.

%% @doc Move forward for Cardinal IPs
-spec ip_forward_cardinal(ip(), rect()) -> ip().
ip_forward_cardinal(#fip{x=X, y=Y} = IP, {{MinX, MinY}, {MaxX, MaxY}}) ->
	if
		X < MinX -> NewX = MaxX+1;
		X > MaxX -> NewX = MinX-1;
		true     -> NewX = X
	end,
	if
		Y < MinY -> NewY = MaxY+1;
		Y > MaxY -> NewY = MinY-1;
		true     -> NewY = Y
	end,
	IP#fip{ x=NewX, y=NewY }.

%% @doc Is X, Y in range of the box created by the second parameter?
-spec is_in_range(coord(), rect()) -> bool().
is_in_range({X, Y}, {{MinX, MinY}, {MaxX, MaxY}}) ->
	if
		X < MinX -> false;
		X > MaxX -> false;
		true     ->
			if
				Y < MinY -> false;
				Y > MaxY -> false;
				true     -> true
			end
	end.

%% @doc Move forward for flying IPs.
-spec ip_forward_flying(ip(), rect()) -> ip().
ip_forward_flying(#fip{x=X, y=Y, dx=DX, dy=DY} = IP, Bounds) ->
	case is_in_range({X, Y}, Bounds) of
		false -> rev_delta(IP);
		true -> ip_forward_flying(IP#fip{ x=X+DX, y=Y+DY }, Bounds)
	end.
