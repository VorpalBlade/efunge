%% @doc Handles manipulation functions for IP.
-module(fip).
-export([getNewPos/1, setDelta/3, revDelta/1]).
-include("fip.hrl").
-include("funge_types.hrl").

%% @spec getNewPos(state()) -> NewState::state()
%% @doc Move IP forward one step.
-spec getNewPos(state()) -> state().
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

%% @spec setDelta(state(), integer(), integer()) -> NewState::state()
%% @doc Set delta in state.
-spec setDelta(state(), integer(), integer()) -> state().
setDelta(#fip{} = State, X, Y) ->
	State#fip{ dx = X, dy = Y }.

%% @spec revDelta(state()) -> NewState::state()
%% @doc Reverse IP.
-spec revDelta(state()) -> state().
revDelta(#fip{} = State) ->
	#fip{dx=DX, dy=DY} = State,
	State#fip{ dx = -DX, dy = -DY }.
