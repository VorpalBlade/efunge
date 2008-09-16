%% @doc Handles manipulation functions for IP.
-module(fip).
-export([getNewPos/2, jump/3,
         setDelta/3, revDelta/1, setOffset/3,
         turnDeltaLeft/1, turnDeltaRight/1,
         findNextMatch/3, findNextNonSpace/2
        ]).
-include("fip.hrl").
-include("funge_types.hrl").
%% @type ip() = #fip{}.
%%    The IP state. See fip.hrl.

%% @spec getNewPos(ip(), fungespace()) -> NewIP::ip()
%% @doc Move IP forward one step.
-spec getNewPos(ip(), fungespace()) -> ip().
getNewPos(#fip{x=X, y=Y, dx=DX, dy=DY} = IP, FungeSpace) ->
	Bounds = fspace:get_bounds(FungeSpace),
	NewX = X+DX,
	NewY = Y+DY,
	NewIP = IP#fip{ x=NewX, y=NewY },
	case is_in_range({NewX, NewY}, Bounds) of
		true -> NewIP;
		false ->
			case isDeltaCardinal(IP) of
				true -> getNewPosCardinal(NewIP, Bounds);
				false -> getNewPosFlying(revDelta(IP), Bounds)
			end
	end.

% @doc Handles j correctly, it will temporarly change delta for the IP then
% jump forward, and finally restore delta.
-spec jump(ip(), fungespace(), integer()) -> ip().
jump(#fip{dx=DX, dy=DY} = IP, FungeSpace, Distance) ->
	IPNewDelta = IP#fip{ dx = DX * Distance, dy = DY * Distance},
	IPNewPos = getNewPos(IPNewDelta, FungeSpace),
	IPNewPos#fip{ dx = DX, dy = DY }.

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


%% @spec setOffset(ip(), integer(), integer()) -> NewState::ip()
%% @doc Set delta in state.
-spec setOffset(ip(), integer(), integer()) -> ip().
setOffset(#fip{} = IP, X, Y) ->
	IP#fip{ offX = X, offY = Y }.

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

%% Private functions

%% @doc Check if IP is cardinal
-spec isDeltaCardinal(ip()) -> bool().
isDeltaCardinal(#fip{dx=DX, dy=DY}) ->
	case {DX, DY} of
		{ 0,  1} -> true;
		{ 0, -1} -> true;
		{ 1,  0} -> true;
		{-1,  0} -> true;
		_ -> false
	end.

%% @doc Move forward for Cardinal IPs
-spec getNewPosCardinal(ip(),{coord(),coord()}) -> ip().
getNewPosCardinal(#fip{x=X, y=Y} = IP, {{MinX, MinY}, {MaxX, MaxY}}) ->
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
-spec is_in_range(coord(),{coord(),coord()}) -> bool().
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
-spec getNewPosFlying(ip(),{coord(),coord()}) -> ip().
getNewPosFlying(#fip{x=X, y=Y, dx=DX, dy=DY} = IP, Bounds) ->
	case is_in_range({X, Y}, Bounds) of
		false -> revDelta(IP);
		true -> getNewPosFlying(IP#fip{ x=X+DX, y=Y+DY }, Bounds)
	end.
