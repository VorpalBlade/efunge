%% @doc Handles manipulation functions for IP.
-module(fip).
-export([getNewPos/2,
         setDelta/3, revDelta/1,
         turnDeltaLeft/1, turnDeltaRight/1,
         findNextMatch/3, findNextNonSpace/2
        ]).
-include("fip.hrl").
-include("fspace.hrl").
-include("funge_types.hrl").

%% @spec getNewPos(ip()) -> NewState::ip()
%% @doc Move IP forward one step.
-spec getNewPos(ip(), fungespace()) -> ip().
getNewPos(#fip{x=X, y=Y, dx=DX, dy=DY} = IP, FungeSpace) ->
	{{MinX, MinY}, {MaxX, MaxY}} = fspace:getBounds(FungeSpace),
	NewX = X+DX,
	NewY = Y+DY,
	if
		NewX < MinX -> NewX2 = MaxX+1;
		NewX > MaxX -> NewX2 = MinX-1;
		true        -> NewX2 = NewX
	end,
	if
		NewY < MinY -> NewY2 = MaxY+1;
		NewY > MaxY -> NewY2 = MinY-1;
		true        -> NewY2 = NewY
	end,
	IP#fip{ x=NewX2, y=NewY2 }.

%% @spec setDelta(ip(), integer(), integer()) -> NewState::ip()
%% @doc Set delta in state.
-spec setDelta(ip(), integer(), integer()) -> ip().
setDelta(#fip{} = IP, X, Y) ->
	IP#fip{ dx = X, dy = Y }.

%% @spec revDelta(ip()) -> NewState::ip()
%% @doc Reverse IP.
-spec revDelta(ip()) -> ip().
revDelta(#fip{dx=DX, dy=DY} = IP) ->
	IP#fip{ dx = -DX, dy = -DY }.

%% @doc Turn IP left.
-spec turnDeltaLeft(ip()) -> ip().
turnDeltaLeft(#fip{dx=DX, dy=DY} = IP) ->
	IP#fip{ dx = DY, dy = -DX }.

%% @doc Turn IP right.
-spec turnDeltaRight(ip()) -> ip().
turnDeltaRight(#fip{dx=DX, dy=DY} = IP) ->
	IP#fip{ dx = -DY, dy = DX }.

%% @doc Search in IP's path for the next time value shows up.
-spec findNextMatch(ip(),integer(),fungespace()) -> ip().
findNextMatch(#fip{x=X, y=Y} = IP, Match, FungeSpace) ->
	Value = fspace:fetch(FungeSpace, {X, Y}),
	if
		Value =:= Match -> IP#fip{x = X, y = Y};
		true -> findNextMatch(getNewPos(IP, FungeSpace), Match, FungeSpace)
	end.

%% @doc Find next one that isn't a whitespace, and isn't in ;;.
-spec findNextNonSpace(ip(),fungespace()) -> {ip(), integer()}.
findNextNonSpace(#fip{x=X, y=Y} = IP, FungeSpace) ->
	Value = fspace:fetch(FungeSpace, {X, Y}),
	if
		Value =:= $\s ->
			findNextNonSpace(getNewPos(IP, FungeSpace), FungeSpace);
		Value =:= $; ->
			IP2 = findNextMatch(getNewPos(IP, FungeSpace), $;, FungeSpace),
			findNextNonSpace(getNewPos(IP2, FungeSpace), FungeSpace);
		true ->
			{IP#fip{x = X, y = Y}, Value}
	end.
