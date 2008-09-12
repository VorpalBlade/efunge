-module(efunge).
-export([start/1, run/1]).
-include("fstate.hrl").
-import(fspace, [set/3, fetch/2]).
-import(fstack, [push/2, peek/1, pop/1, popVec/1, dup/1, swap/1]).

%% @type state() = #fst{}.
%%    The IP and Funge state. See fstate.hrl.

%% @spec run([Filename::string()]) -> none()
%% @doc Handler for -run
run([Filename]) when is_list(Filename) ->
	Retval = start(Filename),
	init:stop(Retval).

%% @spec start(string()) -> int()
%% @doc Load file, set up PRNG, start main loop.
start(Filename) when is_list(Filename) ->
	{R1,R2,R3} = now(),
	random:seed(R1, R2, R3),
	Space = fspace:load(Filename),
	loop(#fst{}, fstack:new(), Space).

%% @spec getNewPos(state()) -> NewState::state()
%% @doc Move IP forward one step.
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

%% @spec setDelta(state(), int(), int()) -> NewState::state()
%% @doc Set delta in state.
setDelta(#fst{} = State, X, Y) ->
	State#fst{ dx = X, dy = Y }.

%% @spec revDelta(state()) -> NewState::state()
%% @doc Reverse IP.
revDelta(#fst{} = State) ->
	#fst{dx=DX, dy=DY} = State,
	State#fst{ dx = -DX, dy = -DY }.


%% @spec fillBuffer(state()) -> {ok, NewState::state()} | {eof, NewState::state()}
%% @doc Fill up the input line buffer if needed.
fillBuffer(#fst{} = State) ->
	StringBuf = State#fst.stringBuffer,
	if
		StringBuf =:= [] ->
			String = io:get_line(''),
			if
				String =:= eof -> {eof, State};
				true ->
					{ok, State#fst{ stringBuffer=String }}
			end;
		true ->
			{ok, State}
	end.

%% @spec readNextChar(state()) -> {NewState, Char}
%% @doc Get a letter from the string buffer.
readNextChar(#fst{} = State) ->
	{Status, NewState} = fillBuffer(State),
	case Status of
		eof -> {State, eof};
		ok ->
			StringBuf = NewState#fst.stringBuffer,
			[H|T] = StringBuf,
			{NewState#fst{ stringBuffer=T }, H}
	end.

%% @spec parseInteger(string()) -> {int(), Rest::string()} | error
%% @doc Parse an integer in a string, return what is left after the end of the
%%      integer, discarding a newlines if there is one directly after the integer.
parseInteger([]) -> error;
parseInteger(String) ->
	Result = string:to_integer(String),
	case Result of
		{error, _Reason} ->
			[_H|T] = String,
			parseInteger(T);
		{Int, Rest} ->
			[H|T] = Rest,
			case H of
				$\n -> {Int, T};
				_ -> Result
			end
	end.


%% @spec readNextInteger(state()) -> {NewState::state(), int()}
%% @doc Get an integer from the string buffer.
readNextInteger(#fst{} = State) ->
	{Status, NewState} = fillBuffer(State),
	case Status of
		eof -> {State, eof};
		ok ->
			StringBuf = NewState#fst.stringBuffer,
			Result = parseInteger(StringBuf),
			case Result of
				%% Try again!
				error ->
					readNextInteger(NewState#fst{ stringBuffer=[] });
				{Int, Rest} ->
					{NewState#fst{ stringBuffer=Rest }, Int}
			end
	end.

%% @spec loop(state(), stack(), tid()) -> int()
%% @doc Main loop
loop(#fst{} = State, Stack, FungeSpace) ->
	Instr = fetch(FungeSpace, {State#fst.x, State#fst.y}),
	case State#fst.isStringMode of
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

%% @spec handleStringMode(int(), state(), stack()) -> {state(), stack()}
%% @doc Handle reading stuff in string mode.
handleStringMode(Instr, #fst{} = State, Stack) ->
	if
		Instr =:= $" ->
			{State#fst{ isStringMode= false }, Stack};
		true ->
			{State, push(Stack, Instr)}
	end.

%% Finally, process instruction:

%% @spec processInstruction(int(), state(), stack(), Space) -> {state(), stack()}
%% @doc Process an instruction.

%% 0-9 Any number.
processInstruction(Instr, #fst{} = State, Stack, _Space) when (Instr >= $0) andalso (Instr =< $9) ->
	{State, push(Stack, Instr - $0)};

%%   Space
processInstruction($\s, #fst{} = State, Stack, _Space) ->
	{State, Stack};

%% p Put
processInstruction($p, #fst{} = State, Stack, Space) ->
	{S1, C} = popVec(Stack),
	{S2, V} = pop(S1),
	set(Space, C, V),
	{State, S2};

%% g Get
processInstruction($g, #fst{} = State, Stack, Space) ->
	{S1, C} = popVec(Stack),
	V = fetch(Space, C),
	{State, push(S1, V)};


%% + Plus
processInstruction($+, #fst{} = State, Stack, _Space) ->
	{S1,B} = pop(Stack), {S2,A} = pop(S1), {State, push(S2, A + B)};
%% - Minus
processInstruction($-, #fst{} = State, Stack, _Space) ->
	{S1,B} = pop(Stack), {S2,A} = pop(S1), {State, push(S2, A - B)};
%% * Multiplication
processInstruction($*, #fst{} = State, Stack, _Space) ->
	{S1,B} = pop(Stack), {S2,A} = pop(S1), {State, push(S2, A * B)};
%% / Integer division
processInstruction($/, #fst{} = State, Stack, _Space) ->
	{S1,B} = pop(Stack),
	{S2,A} = pop(S1),
	if
		B =:= 0 -> {State, push(S2, 0)};
		true    -> {State, push(S2, A div B)}
	end;

%% % Reminder
processInstruction($%, #fst{} = State, Stack, _Space) ->
	{S1,B} = pop(Stack),
	{S2,A} = pop(S1),
	if
		B =:= 0 -> {State, push(S2, 0)};
		true    -> {State, push(S2, A rem B)}
	end;

%% " String mode
processInstruction($", #fst{} = State, Stack, _Space) ->
	{State#fst{ isStringMode=true }, Stack};

%% > East
processInstruction($>, #fst{} = State, Stack, _Space) ->
	{setDelta(State, 1, 0), Stack};
%% < West
processInstruction($<, #fst{} = State, Stack, _Space) ->
	{setDelta(State, -1, 0), Stack};
%% ^ North
processInstruction($^, #fst{} = State, Stack, _Space) ->
	{setDelta(State, 0, -1), Stack};
%% v South
processInstruction($v, #fst{} = State, Stack, _Space) ->
	{setDelta(State, 0, 1), Stack};
%% ? Random direction
processInstruction($?, #fst{} = State, Stack, _Space) ->
	R = random:uniform(4),
	case R of
		1 -> {setDelta(State, -1,  0), Stack};
		2 -> {setDelta(State,  1,  0), Stack};
		3 -> {setDelta(State,  0, -1), Stack};
		4 -> {setDelta(State,  0,  1), Stack}
	end;

%% ! Not
processInstruction($!, #fst{} = State, Stack, _Space) ->
	{S1, V} = pop(Stack),
	if
		V =:= 0 -> R = 1;
		true    -> R = 0
	end,
	{State, push(S1, R)};

%% ` Greater than
processInstruction($`, #fst{} = State, Stack, _Space) ->
	{S1,B} = pop(Stack),
	{S2,A} = pop(S1),
	if
		A > B -> R = 1;
		true  -> R = 0
	end,
	{State, push(S2, R)};

%% : Dup
processInstruction($:, #fst{} = State, Stack, _Space) ->
	{State, dup(Stack)};
%% \ Swap
processInstruction($\\, #fst{} = State, Stack, _Space) ->
	{State, swap(Stack)};
%% $ Pop
processInstruction($$, #fst{} = State, Stack, _Space) ->
	{S1, _} = pop(Stack),
	{State, S1};

%% # Jump
processInstruction($#, #fst{} = State, Stack, _Space) ->
	{getNewPos(State), Stack};

%% _ Horisontal if
processInstruction($_, #fst{} = State, Stack, _Space) ->
	{NewStack, Val} = pop(Stack),
	if
		Val =:= 0 ->
			{setDelta(State, 1, 0), NewStack};
		true ->
			{setDelta(State, -1, 0), NewStack}
	end;
%% | Vertical if
processInstruction($|, #fst{} = State, Stack, _Space) ->
	{NewStack, Val} = pop(Stack),
	if
		Val =:= 0 ->
			{setDelta(State, 0, 1), NewStack};
		true ->
			{setDelta(State, 0, -1), NewStack}
	end;

%% , Put char
processInstruction($, , #fst{} = State, Stack, _Space) ->
	{NewStack, Val} = pop(Stack),
	io:format("~c", [Val]),
	{State, NewStack};
%% . Put number
processInstruction($., #fst{} = State, Stack, _Space) ->
	{NewStack, Val} = pop(Stack),
	io:format("~w ", [Val]),
	{State, NewStack};

%% ~ Get char
processInstruction($~, #fst{} = State, Stack, _Space) ->
	{NewState, Result} = readNextChar(State),
	if
		Result =:= eof -> {revDelta(State), Stack};
		true           -> {NewState, push(Stack, Result)}
	end;
%% & Get int
processInstruction($&, #fst{} = State, Stack, _Space) ->
	{NewState, Result} = readNextInteger(State),
	if
		Result =:= eof -> {revDelta(State), Stack};
		true           -> {NewState, push(Stack, Result)}
	end;

%% unimplemented
processInstruction(_Instr, #fst{} = State, Stack, _Space) ->
	%%io:format("Instruction ~c is not implemented (at x=~w y=~w).~n",
	%%          [_Instr, State#fst.x, State#fst.y]),
	{revDelta(State), Stack}.
