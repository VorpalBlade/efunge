-module(efunge).
-export([main/1]).
-include("fstate.hrl").
-import(fspace, [set/3, fetch/2]).
-import(fstack, [push/2, peek/1, pop/1, dup/1, swap/1]).

%% Loads file and starts main loop.
main([A]) ->
	Space = fspace:load(A),
	loop(#fst{}, [], Space).

%% getNewPos -> NewState
getNewPos(#fst{} = State) ->
	#fst{x=X, y=Y, dx=DX, dy=DY} = State,
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
	NewState = State#fst{ x=NewX2, y=NewY2 },
	NewState.

%% Returns: New state
setDelta(#fst{} = State, X, Y) ->
	State#fst{ dx = X, dy = Y }.

%% Runs main loop
loop(#fst{} = State, Stack, FungeSpace) ->
	Instr = fetch(FungeSpace, {State#fst.x, State#fst.y}),
	case State#fst.isStringMode of
		true ->
			{NewState, NewStack} = handleStringMode(Instr, State, Stack),
			NewFungeSpace = FungeSpace;
		false ->
			{NewState, NewStack, NewFungeSpace} =
				processInstruction(Instr, State, Stack, FungeSpace)
	end,
	
	loop(getNewPos(NewState), NewStack, NewFungeSpace).


%% Returns:
%%   {NewState, NewStack}
handleStringMode(Instr, #fst{} = State, Stack) ->
	if
		Instr =:= $" ->
			{State#fst{ isStringMode= false }, Stack};
		true ->
			{State, push(Stack, Instr)}
	end.

%% Finally, process instruction:
%% Returns: {NewState, NewStack, NewFungeSpace}

%% 0-9 Any number. 
processInstruction(Instr, #fst{} = State, Stack, Space) when (Instr >= $0) andalso (Instr =< $9) ->
	{State, push(Stack, Instr - $0), Space};

%%   Space
processInstruction($\s, #fst{} = State, Stack, Space) ->
	{State, Stack, Space};

%% + Plus
processInstruction($+, #fst{} = State, Stack, Space) ->
	{S1,V1} = pop(Stack), {S2,V2} = pop(S1), {State, push(S2, V1 + V2), Space};
%% - Minus
processInstruction($-, #fst{} = State, Stack, Space) ->
	{S1,V1} = pop(Stack), {S2,V2} = pop(S1), {State, push(S2, V1 - V2), Space};
%% * Multiplication
processInstruction($*, #fst{} = State, Stack, Space) ->
	{S1,V1} = pop(Stack), {S2,V2} = pop(S1), {State, push(S2, V1 * V2), Space};
%% / Integer division
processInstruction($\\, #fst{} = State, Stack, Space) ->
	{S1,V1} = pop(Stack), {S2,V2} = pop(S1), {State, push(S2, V1 div V2), Space};


%% " String mode
processInstruction($", #fst{} = State, Stack, Space) ->
	{State#fst{ isStringMode=true }, Stack, Space};

%% > East
processInstruction($>, #fst{} = State, Stack, Space) ->
	{setDelta(State, 1, 0), Stack, Space};
%% < West
processInstruction($<, #fst{} = State, Stack, Space) ->
	{setDelta(State, -1, 0), Stack, Space};
%% ^ North
processInstruction($^, #fst{} = State, Stack, Space) ->
	{setDelta(State, 0, -1), Stack, Space};
%% v South
processInstruction($v, #fst{} = State, Stack, Space) ->
	{setDelta(State, 0, 1), Stack, Space};

%% : Dup
processInstruction($:, #fst{} = State, Stack, Space) ->
	{State, dup(Stack), Space};

%% # Jump
processInstruction($#, #fst{} = State, Stack, Space) ->
	{getNewPos(State), Stack, Space};

%% _ Horisontal if
processInstruction($_, #fst{} = State, Stack, Space) ->
	{NewStack, Val} = pop(Stack),
	if
		Val =:= 0 ->
			{setDelta(State, 1, 0), NewStack, Space};
		true ->
			{setDelta(State, -1, 0), NewStack, Space}
	end;

%% , Put char
processInstruction($,, #fst{} = State, Stack, Space) ->
	{NewStack, Val} = pop(Stack),
	io:format("~c", [Val]),
	{State, NewStack, Space};

%% @ Exit
processInstruction($@, _, _, _) ->
	exit(normal);

%% unimplemented
processInstruction(Instr, #fst{} = State, _, _) ->
	io:format("Instruction ~c is not implemented (at x=~w y=~w).~n",
	          [Instr, State#fst.x, State#fst.y]),
	exit(notImplemented).
