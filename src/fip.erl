%% @doc Handles manipulation functions for IP.
-module(fip).
-export([getNewPos/1, setDelta/3, revDelta/1]).
-include("fip.hrl").
-include("funge_types.hrl").

%% @spec getNewPos(ip()) -> NewState::ip()
%% @doc Move IP forward one step.
-spec getNewPos(ip()) -> ip().
getNewPos(#fip{} = State) ->
	#fip{x=X, y=Y, dx=DX, dy=DY} = State,
	NewX = X+DX,
	NewY = Y+DY,
	if
		NewX < 0  -> NewX2 = 80;
		NewX > 80 -> NewX2 = 0;
		true      -> NewX2 = NewX
	end,
	if
		NewY < 0  -> NewY2 = 25;
		NewY > 25 -> NewY2 = 0;
		true      -> NewY2 = NewY
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
