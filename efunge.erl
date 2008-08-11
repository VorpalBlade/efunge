-module(efunge).
-export([main/1]).
-include("fstate.hrl").
-import(fspace, [set/3, fetch/2]).
-import(fstack, [push/2, peek/1, pop/1, popVec/1, dup/1, swap/1]).

%% Loads file and starts main loop.
main([A]) ->
	{R1,R2,R3} = now(),
	random:seed(R1, R2, R3),
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
%% Returns: New state
revDelta(#fst{} = State) ->
	#fst{dx=DX, dy=DY} = State,
	State#fst{ dx = -DX, dy = -DY }.

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

%% p Put
processInstruction($p, #fst{} = State, Stack, Space) ->
	{S1, C} = popVec(Stack),
	{S2, V} = pop(S1),
	NewSpace = set(Space, C, V),
	{State, S2, NewSpace};

%% g Get
processInstruction($g, #fst{} = State, Stack, Space) ->
	{S1, C} = popVec(Stack),
	V = fetch(Space, C),
	{State, push(S1, V), Space};


%% + Plus
processInstruction($+, #fst{} = State, Stack, Space) ->
	{S1,B} = pop(Stack), {S2,A} = pop(S1), {State, push(S2, A + B), Space};
%% - Minus
processInstruction($-, #fst{} = State, Stack, Space) ->
	{S1,B} = pop(Stack), {S2,A} = pop(S1), {State, push(S2, A - B), Space};
%% * Multiplication
processInstruction($*, #fst{} = State, Stack, Space) ->
	{S1,B} = pop(Stack), {S2,A} = pop(S1), {State, push(S2, A * B), Space};
%% / Integer division
processInstruction($/, #fst{} = State, Stack, Space) ->
	{S1,B} = pop(Stack),
	{S2,A} = pop(S1),
	if
		B =:= 0 -> {State, push(S2, 0), Space};
		true     -> {State, push(S2, A div B), Space}
	end;

%% % Reminder
processInstruction($%, #fst{} = State, Stack, Space) ->
	{S1,B} = pop(Stack),
	{S2,A} = pop(S1),
	if
		B =:= 0 -> {State, push(S2, 0), Space};
		true     -> {State, push(S2, A rem B), Space}
	end;

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
%% ? Random direction
processInstruction($?, #fst{} = State, Stack, Space) ->
	R = random:uniform(4),
	case R of
		1 -> {setDelta(State, -1,  0), Stack, Space};
		2 -> {setDelta(State,  1,  0), Stack, Space};
		3 -> {setDelta(State,  0, -1), Stack, Space};
		4 -> {setDelta(State,  0,  1), Stack, Space}
	end;

%% ! Not
processInstruction($!, #fst{} = State, Stack, Space) ->
	{S1, V} = pop(Stack),
	if
		V =:= 0 -> R = 1;
		true    -> R = 0
	end,
	{State, push(S1, R), Space};

%% ` Greater than
processInstruction($`, #fst{} = State, Stack, Space) ->
	{S1,B} = pop(Stack),
	{S2,A} = pop(S1),
	if
		A > B -> R = 1;
		true  -> R = 0
	end,
	{State, push(S2, R), Space};

%% : Dup
processInstruction($:, #fst{} = State, Stack, Space) ->
	{State, dup(Stack), Space};
%% \ Swap
processInstruction($\\, #fst{} = State, Stack, Space) ->
	{State, swap(Stack), Space};
%% $ Pop
processInstruction($$, #fst{} = State, Stack, Space) ->
	{S1, _} = pop(Stack),
	{State, S1, Space};

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
%% | Vertical if
processInstruction($|, #fst{} = State, Stack, Space) ->
	{NewStack, Val} = pop(Stack),
	if
		Val =:= 0 ->
			{setDelta(State, 0, 1), NewStack, Space};
		true ->
			{setDelta(State, 0, -1), NewStack, Space}
	end;

%% , Put char
processInstruction($, , #fst{} = State, Stack, Space) ->
	{NewStack, Val} = pop(Stack),
	io:format("~c", [Val]),
	{State, NewStack, Space};
%% . Put number
processInstruction($., #fst{} = State, Stack, Space) ->
	{NewStack, Val} = pop(Stack),
	io:format("~w ", [Val]),
	{State, NewStack, Space};
%% ~ Get char
processInstruction($~, #fst{} = State, Stack, Space) ->
	Result = io:fread("", "~1c"),
	if
		Result =:= eof -> {revDelta(State), Stack, Space};
		true ->
			{ok, [[I]]} = Result,
			{State, push(Stack, I), Space}
	end;
%% & Get int
processInstruction($&, #fst{} = State, Stack, Space) ->
	Result = io:fread("", "~d"),
	if
		Result =:= eof -> {revDelta(State), Stack, Space};
		true ->
			{ok, [I]} = Result,
			{State, push(Stack, I), Space}
	end;

%% @ Exit
processInstruction($@, _, _, _) ->
	exit(normal);

%% unimplemented
processInstruction(Instr, #fst{} = State, Stack, Space) ->
	io:format("Instruction ~c is not implemented (at x=~w y=~w).~n",
	          [Instr, State#fst.x, State#fst.y]),
	{revDelta(State), Stack, Space},
	exit(notImplemented).
