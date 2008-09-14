%% @doc Handles main loop, and will handle exeuting an instruction from
%% elsewhere.
-module(finterpreter).
-export([loop/3]).
-include("fip.hrl").
-include("fspace.hrl").
-include("funge_types.hrl").
-import(fspace, [set/3, fetch/2]).
-import(fstackstack, [push/2, peek/1, pop/1, popVec/1, pushVec/2, dup/1, swap/1]).
-import(finput, [readNextChar/1, readNextInteger/1]).
-import(fip, [getNewPos/2, setDelta/3, setOffset/3, revDelta/1, turnDeltaLeft/1, turnDeltaRight/1]).

%% @spec loop(ip(), stackstack(), tid()) -> integer()
%% @doc Main loop
-spec loop(ip(), stackstack(), fungespace()) -> integer().
loop(#fip{} = IP, Stack, #fspace{} = FungeSpace) ->
	Instr = fetch(FungeSpace, {IP#fip.x, IP#fip.y}),
	%io:format("~c (x=~w y=~w)~n", [Instr, IP#fip.x, IP#fip.y]),
	case IP#fip.isStringMode of
		true ->
			{NewIP, NewStack} = handleStringMode(Instr, IP, Stack),
			loop(getNewPos(NewIP, FungeSpace), NewStack, FungeSpace);
		false ->
			%% Handle @ specically since we need to end loop then.
			if
				Instr =:= $@ ->
					fspace:delete(FungeSpace),
					0;
				Instr =:= $q ->
					{_, Retval} = pop(Stack),
					fspace:delete(FungeSpace),
					Retval;
				true ->
					{NewIP, NewStack} =
						processInstruction(Instr, IP, Stack, FungeSpace),
					loop(getNewPos(NewIP, FungeSpace), NewStack, FungeSpace)
			end
	end.

%% @spec handleStringMode(integer(), ip(), stackstack()) -> {ip(), stack()}
%% @doc Handle reading stuff in string mode.
-spec handleStringMode(integer(),ip(),stackstack()) -> {ip(),stack()}.
handleStringMode(Instr, #fip{ lastWasSpace = LastSpace } = IP, Stack) ->
	if
		Instr =:= $\s andalso not LastSpace ->
			{IP#fip{ lastWasSpace=true }, push(Stack, Instr)};
		Instr =:= $\s ->
			{IP, Stack};
		Instr =:= $" ->
			{IP#fip{ isStringMode=false, lastWasSpace=false }, Stack};
		true ->
			{IP#fip{ lastWasSpace=false }, push(Stack, Instr)}
	end.

%% Finally, process instruction:

%% @spec processInstruction(integer(), ip(), stack(), Space) -> {ip(), stack()}
%% @doc Process an instruction.
-spec processInstruction(integer(),ip(),stackstack(), fungespace()) -> {ip(),stack()}.

%%   Space
processInstruction($\s, #fip{} = IP, Stack, _Space) ->
	{IP, Stack};

%% p Put
processInstruction($p, #fip{} = IP, Stack, #fspace{} = Space) ->
	{S1, C} = popVec(Stack),
	{S2, V} = pop(S1),
	set(Space, C, V),
	{IP, S2};

%% g Get
processInstruction($g, #fip{} = IP, Stack, #fspace{} = Space) ->
	{S1, C} = popVec(Stack),
	V = fetch(Space, C),
	{IP, push(S1, V)};


%% + Plus
processInstruction($+, #fip{} = IP, Stack, _Space) ->
	{S1,B} = pop(Stack), {S2,A} = pop(S1), {IP, push(S2, A + B)};
%% - Minus
processInstruction($-, #fip{} = IP, Stack, _Space) ->
	{S1,B} = pop(Stack), {S2,A} = pop(S1), {IP, push(S2, A - B)};
%% * Multiplication
processInstruction($*, #fip{} = IP, Stack, _Space) ->
	{S1,B} = pop(Stack), {S2,A} = pop(S1), {IP, push(S2, A * B)};
%% / Integer division
processInstruction($/, #fip{} = IP, Stack, _Space) ->
	{S1,B} = pop(Stack),
	{S2,A} = pop(S1),
	if
		B =:= 0 -> {IP, push(S2, 0)};
		true    -> {IP, push(S2, A div B)}
	end;

%% % Reminder
processInstruction($%, #fip{} = IP, Stack, _Space) ->
	{S1,B} = pop(Stack),
	{S2,A} = pop(S1),
	if
		B =:= 0 -> {IP, push(S2, 0)};
		true    -> {IP, push(S2, A rem B)}
	end;

%% " String mode
processInstruction($", #fip{} = IP, Stack, _Space) ->
	{IP#fip{ isStringMode=true }, Stack};

%% > East
processInstruction($>, #fip{} = IP, Stack, _Space) ->
	{setDelta(IP, 1, 0), Stack};
%% < West
processInstruction($<, #fip{} = IP, Stack, _Space) ->
	{setDelta(IP, -1, 0), Stack};
%% ^ North
processInstruction($^, #fip{} = IP, Stack, _Space) ->
	{setDelta(IP, 0, -1), Stack};
%% v South
processInstruction($v, #fip{} = IP, Stack, _Space) ->
	{setDelta(IP, 0, 1), Stack};
%% ? Random direction
processInstruction($?, #fip{} = IP, Stack, _Space) ->
	R = random:uniform(4),
	case R of
		1 -> {setDelta(IP, -1,  0), Stack};
		2 -> {setDelta(IP,  1,  0), Stack};
		3 -> {setDelta(IP,  0, -1), Stack};
		4 -> {setDelta(IP,  0,  1), Stack}
	end;

%% ! Not
processInstruction($!, #fip{} = IP, Stack, _Space) ->
	{S1, V} = pop(Stack),
	if
		V =:= 0 -> R = 1;
		true    -> R = 0
	end,
	{IP, push(S1, R)};

%% ` Greater than
processInstruction($`, #fip{} = IP, Stack, _Space) ->
	{S1,B} = pop(Stack),
	{S2,A} = pop(S1),
	if
		A > B -> R = 1;
		true  -> R = 0
	end,
	{IP, push(S2, R)};

%% : Dup
processInstruction($:, #fip{} = IP, Stack, _Space) ->
	{IP, dup(Stack)};
%% \ Swap
processInstruction($\\, #fip{} = IP, Stack, _Space) ->
	{IP, swap(Stack)};
%% $ Pop
processInstruction($$, #fip{} = IP, Stack, _Space) ->
	{S1, _} = pop(Stack),
	{IP, S1};

%% # Jump
processInstruction($#, #fip{} = IP, Stack, #fspace{} = Space) ->
	{getNewPos(IP, Space), Stack};

%% _ Horisontal if
processInstruction($_, #fip{} = IP, Stack, _Space) ->
	{NewStack, Val} = pop(Stack),
	if
		Val =:= 0 ->
			{setDelta(IP, 1, 0), NewStack};
		true ->
			{setDelta(IP, -1, 0), NewStack}
	end;
%% | Vertical if
processInstruction($|, #fip{} = IP, Stack, _Space) ->
	{NewStack, Val} = pop(Stack),
	if
		Val =:= 0 ->
			{setDelta(IP, 0, 1), NewStack};
		true ->
			{setDelta(IP, 0, -1), NewStack}
	end;

%% , Put char
processInstruction($, , #fip{} = IP, Stack, _Space) ->
	{NewStack, Val} = pop(Stack),
	io:format("~c", [Val]),
	{IP, NewStack};
%% . Put number
processInstruction($., #fip{} = IP, Stack, _Space) ->
	{NewStack, Val} = pop(Stack),
	io:format("~w ", [Val]),
	{IP, NewStack};

%% ~ Get char
processInstruction($~, #fip{} = IP, Stack, _Space) ->
	{NewIP, Result} = readNextChar(IP),
	if
		Result =:= eof -> {revDelta(IP), Stack};
		true           -> {NewIP, push(Stack, Result)}
	end;
%% & Get int
processInstruction($&, #fip{} = IP, Stack, _Space) ->
	{NewIP, Result} = readNextInteger(IP),
	if
		Result =:= eof -> {revDelta(IP), Stack};
		true           -> {NewIP, push(Stack, Result)}
	end;


%% Begin Funge-98 instructions.

%% [ Turn Left
processInstruction($[, #fip{} = IP, Stack, _Space) ->
	{turnDeltaLeft(IP), Stack};
%% ] Turn Right
processInstruction($], #fip{} = IP, Stack, _Space) ->
	{turnDeltaRight(IP), Stack};

%% ;
processInstruction($;, #fip{} = IP, Stack, Space) ->
	{fip:findNextMatch(getNewPos(IP, Space), $;, Space), Stack};

%% k Iterate
processInstruction($k, #fip{} = IP, Stack, Space) ->
	{S1, Count} = pop(Stack),
	if
		Count < 0 ->
			{revDelta(IP), S1};
		Count =:= 0 ->
			{getNewPos(IP, Space), S1};
		true ->
			{_, Instr} = fip:findNextNonSpace(getNewPos(IP, Space), Space),
			iterate(Count, Instr, IP, S1, Space)
	end;

%% ' Fetch char
processInstruction($', #fip{} = IP, Stack, Space) ->
	#fip{ x = X, y = Y} = NewIP = getNewPos(IP, Space),
	Value = fetch(Space, {X, Y}),
	{NewIP, push(Stack, Value)};

%% s Set char
processInstruction($s, #fip{} = IP, Stack, Space) ->
	#fip{ x = X, y = Y} = NewIP = getNewPos(IP, Space),
	{S1, Value} = pop(Stack),
	set(Space, {X, Y}, Value),
	{NewIP, S1};


%% n Clear Stack
processInstruction($n, #fip{} = IP, Stack, _Space) ->
	{IP, fstackstack:clear(Stack)};

%% w Compare
processInstruction($w, #fip{} = IP, Stack, _Space) ->
	{S1, B} = pop(Stack),
	{S2, A} = pop(S1),
	if
		A < B ->
			{turnDeltaLeft(IP), S2};
		A > B ->
			{turnDeltaRight(IP), S2};
		true ->
			{IP, S2}
	end;

%% x Absolute delta
processInstruction($x, #fip{} = IP, Stack, _Space) ->
	{S1, {X, Y}} = popVec(Stack),
	{setDelta(IP, X,  Y), S1};

%% j Jump
processInstruction($j, #fip{} = IP, Stack, Space) ->
	{S1, Dist} = pop(Stack),
	{fip:jump(IP, Space, Dist), S1};


%% r Reflect
processInstruction($r, #fip{} = IP, Stack, _Space) ->
	{revDelta(IP), Stack};
%% z NOP
processInstruction($z, #fip{} = IP, Stack, _Space) ->
	{IP, Stack};

%% { Begin Stack
processInstruction(${, #fip{ x = X, y = Y, dx = DX, dy = DY, offX = OX, offY = OY} = IP, StackStack, _Space) ->
	{S1, N} = pop(StackStack),
	S2 = pushVec(S1, {OX, OY}),
	S3 = fstackstack:ssBegin(S2, N),
	IP2 = setOffset(IP, X+DX, Y+DY),
	{IP2, S3};
% %% } End Stack
% processInstruction(${, #fip{} = IP, Stack, _Space) ->
% 	{IP, Stack};
% %% u Stack under Stack
% processInstruction(${, #fip{} = IP, Stack, _Space) ->
% 	{IP, Stack};



%% Handle ranges and unimplemented.

%% 0-9 Any number.
processInstruction(Instr, #fip{} = IP, Stack, _Space) when (Instr >= $0) andalso (Instr =< $9) ->
	{IP, push(Stack, Instr - $0)};
%% a-f Hexdecimal numbers.
processInstruction(Instr, #fip{} = IP, Stack, _Space) when (Instr >= $a) andalso (Instr =< $f) ->
	{IP, push(Stack, Instr - $a + 10)};

%% unimplemented
processInstruction(_Instr, #fip{} = IP, Stack, _Space) ->
	io:format("Instruction ~c is not implemented (at x=~w y=~w).~n",
	          [_Instr, IP#fip.x, IP#fip.y]),
	{revDelta(IP), Stack}.


%% @doc Iterate helper
-spec iterate(non_neg_integer(),integer(),ip(),stackstack(),fungespace()) -> {ip(),stack()}.
iterate(0, _Instr, IP, Stack, _Space) ->
	{IP, Stack};
iterate(Count, Instr, IP, Stack, Space) ->
	{IP2, Stack2} = processInstruction(Instr, IP, Stack, Space),
	iterate(Count-1, Instr, IP2, Stack2, Space).
