%% @doc Handles manipulation functions for IP.
-module(fip).
-export([getNewPos/2, setDelta/3, revDelta/1, ipTurnLeft/1, ipTurnRight/1]).
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
		NewX < MinX -> NewX2 = MaxX;
		NewX > MaxX -> NewX2 = MinX;
		true        -> NewX2 = NewX
	end,
	if
		NewY < MinY -> NewY2 = MaxY;
		NewY > MaxY -> NewY2 = MinY;
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

-spec ipTurnLeft(ip()) -> ip().
ipTurnLeft(#fip{dx=DX, dy=DY} = IP) ->
	IP#fip{ dx = DY, dy = -DX }.

-spec ipTurnRight(ip()) -> ip().
ipTurnRight(#fip{dx=DX, dy=DY} = IP) ->
	IP#fip{ dx = -DY, dy = DX }.
