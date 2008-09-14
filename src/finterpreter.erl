-module(finterpreter).
-export([loop/3, processInstruction/4]).
-include("fip.hrl").
-include("funge_types.hrl").
-import(fspace, [set/3, fetch/2]).
-import(fstack, [push/2, peek/1, pop/1, popVec/1, dup/1, swap/1]).
-import(finput, [readNextChar/1, readNextInteger/1]).
-import(fip, [getNewPos/1, setDelta/3, revDelta/1]).

%% @spec loop(state(), stack(), tid()) -> integer()
%% @doc Main loop
-spec loop(state(), stack(), integer()) -> integer().
loop(#fip{} = State, Stack, FungeSpace) ->
	Instr = fetch(FungeSpace, {State#fip.x, State#fip.y}),
	case State#fip.isStringMode of
		true ->
			{NewState, NewStack} = handleStringMode(Instr, State, Stack),
			loop(getNewPos(NewState), NewStack, FungeSpace);
		false ->
			%% Handle @ specically since we need to end loop then.
			if
				Instr =:= $@ ->
					fspace:delete(FungeSpace),
					0;
				true ->
					{NewState, NewStack} =
						processInstruction(Instr, State, Stack, FungeSpace),
					loop(getNewPos(NewState), NewStack, FungeSpace)
			end
	end.

%% @spec handleStringMode(integer(), state(), stack()) -> {state(), stack()}
%% @doc Handle reading stuff in string mode.
-spec handleStringMode(integer(),state(),stack()) -> {state(),stack()}.
handleStringMode(Instr, #fip{} = State, Stack) ->
	if
		Instr =:= $" ->
			{State#fip{ isStringMode= false }, Stack};
		true ->
			{State, push(Stack, Instr)}
	end.

%% Finally, process instruction:

%% @spec processInstruction(integer(), state(), stack(), Space) -> {state(), stack()}
%% @doc Process an instruction.
-spec processInstruction(integer(),state(),stack(), fungespace()) -> {state(),stack()}.

%%   Space
processInstruction($\s, #fip{} = State, Stack, _Space) ->
	{State, Stack};

%% p Put
processInstruction($p, #fip{} = State, Stack, Space) ->
	{S1, C} = popVec(Stack),
	{S2, V} = pop(S1),
	set(Space, C, V),
	{State, S2};

%% g Get
processInstruction($g, #fip{} = State, Stack, Space) ->
	{S1, C} = popVec(Stack),
	V = fetch(Space, C),
	{State, push(S1, V)};


%% + Plus
processInstruction($+, #fip{} = State, Stack, _Space) ->
	{S1,B} = pop(Stack), {S2,A} = pop(S1), {State, push(S2, A + B)};
%% - Minus
processInstruction($-, #fip{} = State, Stack, _Space) ->
	{S1,B} = pop(Stack), {S2,A} = pop(S1), {State, push(S2, A - B)};
%% * Multiplication
processInstruction($*, #fip{} = State, Stack, _Space) ->
	{S1,B} = pop(Stack), {S2,A} = pop(S1), {State, push(S2, A * B)};
%% / Integer division
processInstruction($/, #fip{} = State, Stack, _Space) ->
	{S1,B} = pop(Stack),
	{S2,A} = pop(S1),
	if
		B =:= 0 -> {State, push(S2, 0)};
		true    -> {State, push(S2, A div B)}
	end;

%% % Reminder
processInstruction($%, #fip{} = State, Stack, _Space) ->
	{S1,B} = pop(Stack),
	{S2,A} = pop(S1),
	if
		B =:= 0 -> {State, push(S2, 0)};
		true    -> {State, push(S2, A rem B)}
	end;

%% " String mode
processInstruction($", #fip{} = State, Stack, _Space) ->
	{State#fip{ isStringMode=true }, Stack};

%% > East
processInstruction($>, #fip{} = State, Stack, _Space) ->
	{setDelta(State, 1, 0), Stack};
%% < West
processInstruction($<, #fip{} = State, Stack, _Space) ->
	{setDelta(State, -1, 0), Stack};
%% ^ North
processInstruction($^, #fip{} = State, Stack, _Space) ->
	{setDelta(State, 0, -1), Stack};
%% v South
processInstruction($v, #fip{} = State, Stack, _Space) ->
	{setDelta(State, 0, 1), Stack};
%% ? Random direction
processInstruction($?, #fip{} = State, Stack, _Space) ->
	R = random:uniform(4),
	case R of
		1 -> {setDelta(State, -1,  0), Stack};
		2 -> {setDelta(State,  1,  0), Stack};
		3 -> {setDelta(State,  0, -1), Stack};
		4 -> {setDelta(State,  0,  1), Stack}
	end;

%% ! Not
processInstruction($!, #fip{} = State, Stack, _Space) ->
	{S1, V} = pop(Stack),
	if
		V =:= 0 -> R = 1;
		true    -> R = 0
	end,
	{State, push(S1, R)};

%% ` Greater than
processInstruction($`, #fip{} = State, Stack, _Space) ->
	{S1,B} = pop(Stack),
	{S2,A} = pop(S1),
	if
		A > B -> R = 1;
		true  -> R = 0
	end,
	{State, push(S2, R)};

%% : Dup
processInstruction($:, #fip{} = State, Stack, _Space) ->
	{State, dup(Stack)};
%% \ Swap
processInstruction($\\, #fip{} = State, Stack, _Space) ->
	{State, swap(Stack)};
%% $ Pop
processInstruction($$, #fip{} = State, Stack, _Space) ->
	{S1, _} = pop(Stack),
	{State, S1};

%% # Jump
processInstruction($#, #fip{} = State, Stack, _Space) ->
	{getNewPos(State), Stack};

%% _ Horisontal if
processInstruction($_, #fip{} = State, Stack, _Space) ->
	{NewStack, Val} = pop(Stack),
	if
		Val =:= 0 ->
			{setDelta(State, 1, 0), NewStack};
		true ->
			{setDelta(State, -1, 0), NewStack}
	end;
%% | Vertical if
processInstruction($|, #fip{} = State, Stack, _Space) ->
	{NewStack, Val} = pop(Stack),
	if
		Val =:= 0 ->
			{setDelta(State, 0, 1), NewStack};
		true ->
			{setDelta(State, 0, -1), NewStack}
	end;

%% , Put char
processInstruction($, , #fip{} = State, Stack, _Space) ->
	{NewStack, Val} = pop(Stack),
	io:format("~c", [Val]),
	{State, NewStack};
%% . Put number
processInstruction($., #fip{} = State, Stack, _Space) ->
	{NewStack, Val} = pop(Stack),
	io:format("~w ", [Val]),
	{State, NewStack};

%% ~ Get char
processInstruction($~, #fip{} = State, Stack, _Space) ->
	{NewState, Result} = readNextChar(State),
	if
		Result =:= eof -> {revDelta(State), Stack};
		true           -> {NewState, push(Stack, Result)}
	end;
%% & Get int
processInstruction($&, #fip{} = State, Stack, _Space) ->
	{NewState, Result} = readNextInteger(State),
	if
		Result =:= eof -> {revDelta(State), Stack};
		true           -> {NewState, push(Stack, Result)}
	end;

%% 0-9 Any number.
processInstruction(Instr, #fip{} = State, Stack, _Space) when (Instr >= $0) andalso (Instr =< $9) ->
	{State, push(Stack, Instr - $0)};

%% unimplemented
processInstruction(_Instr, #fip{} = State, Stack, _Space) ->
	%%io:format("Instruction ~c is not implemented (at x=~w y=~w).~n",
	%%          [_Instr, State#fip.x, State#fip.y]),
	{revDelta(State), Stack}.
