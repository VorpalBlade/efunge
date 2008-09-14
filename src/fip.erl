%% @doc Handles manipulation functions for IP.
-module(fip).
-export([getNewPos/2, setDelta/3, revDelta/1]).
-include("fip.hrl").
-include("fspace.hrl").
-include("funge_types.hrl").

%% @spec getNewPos(ip()) -> NewState::ip()
%% @doc Move IP forward one step.
-spec getNewPos(ip(), fungespace()) -> ip().
getNewPos(#fip{} = State, FungeSpace) ->
	#fip{x=X, y=Y, dx=DX, dy=DY} = State,
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
	State#fip{ x=NewX2, y=NewY2 }.

%% @spec setDelta(ip(), integer(), integer()) -> NewState::ip()
%% @doc Set delta in state.
-spec setDelta(ip(), integer(), integer()) -> ip().
setDelta(#fip{} = State, X, Y) ->
	State#fip{ dx = X, dy = Y }.

%% @spec revDelta(ip()) -> NewState::ip()
%% @doc Reverse IP.
-spec revDelta(ip()) -> ip().
revDelta(#fip{} = State) ->
	#fip{dx=DX, dy=DY} = State,
	State#fip{ dx = -DX, dy = -DY }.
