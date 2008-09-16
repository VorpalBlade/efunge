%% @doc Handles main loop, and will handle exeuting an instruction from
%% elsewhere.
-module(finterpreter).
-export([loop/3]).
-include("fip.hrl").
-include("funge_types.hrl").
-import(fspace, [set/4, fetch/3]).
-import(fstackstack, [push/2, pop/1, popVec/1, dup/1, swap/1]).
-import(finput, [read_next_char/1, read_next_integer/1]).
-import(fip, [ip_forward/2, set_delta/3, set_offset/3, rev_delta/1, turn_delta_left/1, turn_delta_right/1]).

%% @type process_instr_ret() = {ip(),stack()} | {dead, integer()}.
-type process_instr_ret() :: {ip(),stack()} | {dead, integer()}.

%% @spec loop(ip(), stackstack(), tid()) -> integer()
%% @doc Main loop
-spec loop(ip(), stackstack(), fungespace()) -> integer().
loop(#fip{} = IP, Stack, FungeSpace) ->
	Instr = fspace:fetch(FungeSpace, {IP#fip.x, IP#fip.y}),
	%io:format("~c", [Instr]),
	%io:format("~c (x=~w y=~w)~n", [Instr, IP#fip.x, IP#fip.y]),
	case IP#fip.isStringMode of
		true ->
			{NewIP, NewStack} = handle_string_mode(Instr, IP, Stack),
			loop(ip_forward(NewIP, FungeSpace), NewStack, FungeSpace);
		false ->
			case process_instruction(Instr, IP, Stack, FungeSpace) of
				% This is for @ and q.
				{dead, Retval} ->
					fspace:delete(FungeSpace),
					Retval;
				{NewIP, NewStack} ->
					loop(ip_forward(NewIP, FungeSpace), NewStack, FungeSpace)
			end
	end.

%% @spec handle_string_mode(integer(), ip(), stackstack()) -> {ip(), stack()}
%% @doc Handle reading stuff in string mode.
-spec handle_string_mode(integer(),ip(),stackstack()) -> {ip(),stack()}.
handle_string_mode(Instr, #fip{ lastWasSpace = LastSpace } = IP, Stack) ->
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

%% @spec process_instruction(integer(), ip(), stackstack(), Space) -> process_instr_ret()
%% @doc Process an instruction.
-spec process_instruction(integer(),ip(),stackstack(), fungespace()) -> process_instr_ret().

%%   Space
process_instruction($\s, #fip{} = IP, Stack, _Space) ->
	{IP, Stack};

%% p Put
process_instruction($p, #fip{} = IP, Stack, Space) ->
	{S1, C} = popVec(Stack),
	{S2, V} = pop(S1),
	set(Space, IP, C, V),
	{IP, S2};

%% g Get
process_instruction($g, #fip{} = IP, Stack, Space) ->
	{S1, C} = popVec(Stack),
	V = fetch(Space, IP, C),
	{IP, push(S1, V)};


%% + Plus
process_instruction($+, #fip{} = IP, Stack, _Space) ->
	{S1,B} = pop(Stack), {S2,A} = pop(S1), {IP, push(S2, A + B)};
%% - Minus
process_instruction($-, #fip{} = IP, Stack, _Space) ->
	{S1,B} = pop(Stack), {S2,A} = pop(S1), {IP, push(S2, A - B)};
%% * Multiplication
process_instruction($*, #fip{} = IP, Stack, _Space) ->
	{S1,B} = pop(Stack), {S2,A} = pop(S1), {IP, push(S2, A * B)};
%% / Integer division
process_instruction($/, #fip{} = IP, Stack, _Space) ->
	{S1,B} = pop(Stack),
	{S2,A} = pop(S1),
	if
		B =:= 0 -> {IP, push(S2, 0)};
		true    -> {IP, push(S2, A div B)}
	end;

%% % Reminder
process_instruction($%, #fip{} = IP, Stack, _Space) ->
	{S1,B} = pop(Stack),
	{S2,A} = pop(S1),
	if
		B =:= 0 -> {IP, push(S2, 0)};
		true    -> {IP, push(S2, A rem B)}
	end;

%% " String mode
process_instruction($", #fip{} = IP, Stack, _Space) ->
	{IP#fip{ isStringMode=true }, Stack};

%% > East
process_instruction($>, #fip{} = IP, Stack, _Space) ->
	{set_delta(IP, 1, 0), Stack};
%% < West
process_instruction($<, #fip{} = IP, Stack, _Space) ->
	{set_delta(IP, -1, 0), Stack};
%% ^ North
process_instruction($^, #fip{} = IP, Stack, _Space) ->
	{set_delta(IP, 0, -1), Stack};
%% v South
process_instruction($v, #fip{} = IP, Stack, _Space) ->
	{set_delta(IP, 0, 1), Stack};
%% ? Random direction
process_instruction($?, #fip{} = IP, Stack, _Space) ->
	R = random:uniform(4),
	case R of
		1 -> {set_delta(IP, -1,  0), Stack};
		2 -> {set_delta(IP,  1,  0), Stack};
		3 -> {set_delta(IP,  0, -1), Stack};
		4 -> {set_delta(IP,  0,  1), Stack}
	end;

%% ! Not
process_instruction($!, #fip{} = IP, Stack, _Space) ->
	{S1, V} = pop(Stack),
	if
		V =:= 0 -> R = 1;
		true    -> R = 0
	end,
	{IP, push(S1, R)};

%% ` Greater than
process_instruction($`, #fip{} = IP, Stack, _Space) ->
	{S1,B} = pop(Stack),
	{S2,A} = pop(S1),
	if
		A > B -> R = 1;
		true  -> R = 0
	end,
	{IP, push(S2, R)};

%% : Dup
process_instruction($:, #fip{} = IP, Stack, _Space) ->
	{IP, dup(Stack)};
%% \ Swap
process_instruction($\\, #fip{} = IP, Stack, _Space) ->
	{IP, swap(Stack)};
%% $ Pop
process_instruction($$, #fip{} = IP, Stack, _Space) ->
	{S1, _} = pop(Stack),
	{IP, S1};

%% # Jump
process_instruction($#, #fip{} = IP, Stack, Space) ->
	{ip_forward(IP, Space), Stack};

%% _ Horisontal if
process_instruction($_, #fip{} = IP, Stack, _Space) ->
	{NewStack, Val} = pop(Stack),
	if
		Val =:= 0 ->
			{set_delta(IP, 1, 0), NewStack};
		true ->
			{set_delta(IP, -1, 0), NewStack}
	end;
%% | Vertical if
process_instruction($|, #fip{} = IP, Stack, _Space) ->
	{NewStack, Val} = pop(Stack),
	if
		Val =:= 0 ->
			{set_delta(IP, 0, 1), NewStack};
		true ->
			{set_delta(IP, 0, -1), NewStack}
	end;

%% , Put char
process_instruction($, , #fip{} = IP, Stack, _Space) ->
	{NewStack, Val} = pop(Stack),
	io:format("~c", [Val]),
	{IP, NewStack};
%% . Put number
process_instruction($., #fip{} = IP, Stack, _Space) ->
	{NewStack, Val} = pop(Stack),
	io:format("~w ", [Val]),
	{IP, NewStack};

%% ~ Get char
process_instruction($~, #fip{} = IP, Stack, _Space) ->
	{NewIP, Result} = read_next_char(IP),
	if
		Result =:= eof -> {rev_delta(IP), Stack};
		true           -> {NewIP, push(Stack, Result)}
	end;
%% & Get int
process_instruction($&, #fip{} = IP, Stack, _Space) ->
	{NewIP, Result} = read_next_integer(IP),
	if
		Result =:= eof -> {rev_delta(IP), Stack};
		true           -> {NewIP, push(Stack, Result)}
	end;

%% @ Exit
process_instruction($@, _IP, _Stack, _Space) ->
	{dead, 0};


%% Begin Funge-98 instructions.

%% [ Turn Left
process_instruction($[, #fip{} = IP, Stack, _Space) ->
	{turn_delta_left(IP), Stack};
%% ] Turn Right
process_instruction($], #fip{} = IP, Stack, _Space) ->
	{turn_delta_right(IP), Stack};

%% ;
process_instruction($;, #fip{} = IP, Stack, Space) ->
	{fip:find_next_match(ip_forward(IP, Space), $;, Space), Stack};

%% k Iterate
process_instruction($k, #fip{} = IP, Stack, Space) ->
	{S1, Count} = pop(Stack),
	if
		Count < 0 ->
			{rev_delta(IP), S1};
		Count =:= 0 ->
			{IP2, _} = fip:find_next_non_space(ip_forward(IP, Space), Space),
			{IP2, S1};
		true ->
			{_, Instr} = fip:find_next_non_space(ip_forward(IP, Space), Space),
			iterate(Count, Instr, IP, S1, Space)
	end;

%% ' Fetch char
process_instruction($', #fip{} = IP, Stack, Space) ->
	#fip{ x = X, y = Y} = NewIP = ip_forward(IP, Space),
	Value = fspace:fetch(Space, {X, Y}),
	{NewIP, push(Stack, Value)};

%% s Set char
process_instruction($s, #fip{} = IP, Stack, Space) ->
	#fip{ x = X, y = Y} = NewIP = ip_forward(IP, Space),
	{S1, Value} = pop(Stack),
	fspace:set(Space, {X, Y}, Value),
	{NewIP, S1};

%% n Clear Stack
process_instruction($n, #fip{} = IP, Stack, _Space) ->
	{IP, fstackstack:clear(Stack)};

%% w Compare
process_instruction($w, #fip{} = IP, Stack, _Space) ->
	{S1, B} = pop(Stack),
	{S2, A} = pop(S1),
	if
		A < B ->
			{turn_delta_left(IP), S2};
		A > B ->
			{turn_delta_right(IP), S2};
		true ->
			{IP, S2}
	end;

%% x Absolute delta
process_instruction($x, #fip{} = IP, Stack, _Space) ->
	{S1, {X, Y}} = popVec(Stack),
	{set_delta(IP, X,  Y), S1};

%% j Jump
process_instruction($j, #fip{} = IP, Stack, Space) ->
	{S1, Dist} = pop(Stack),
	{fip:jump(IP, Space, Dist), S1};


%% r Reflect
process_instruction($r, #fip{} = IP, Stack, _Space) ->
	{rev_delta(IP), Stack};
%% z NOP
process_instruction($z, #fip{} = IP, Stack, _Space) ->
	{IP, Stack};

%% { Begin Stack
process_instruction(${, #fip{ x = X, y = Y, dx = DX, dy = DY, offX = OX, offY = OY} = IP, StackStack, _Space) ->
	{S1, N} = pop(StackStack),
	S2 = fstackstack:ssBegin(S1, N),
	S3 = fstackstack:pushVecSOSS(S2, {OX, OY}),
	IP2 = set_offset(IP, X+DX, Y+DY),
	{IP2, S3};

%% } End Stack
process_instruction($}, #fip{} = IP, StackStack, _Space) ->
	{S1, N} = pop(StackStack),
	try
		{S2, {OX, OY}} = fstackstack:popVecSOSS(S1),
		S3 = fstackstack:ssEnd(S2, N),
		IP2 = set_offset(IP, OX, OY),
		{IP2, S3}
	catch
		throw:oneStack -> {rev_delta(IP), S1}
	end;

%% u Stack under Stack
process_instruction($u, #fip{} = IP, StackStack, _Space) ->
	{S1, Count} = pop(StackStack),
	try
		S2 = fstackstack:ssUnder(S1, Count),
		{IP, S2}
	catch
		throw:oneStack -> {rev_delta(IP), S1}
	end;

%% y System Info
process_instruction($y, #fip{} = IP, Stack, Space) ->
	{S1, N} = pop(Stack),
	{IP, fsysinfo:sysInfo(N, IP, S1, Space)};

%% ( Load fingerprint
process_instruction($(, #fip{} = IP, StackStack, _Space) ->
	{S1, N} = pop(StackStack),
	if
		N < 0 -> {rev_delta(IP), S1};
		true ->
			[TOSS|T] = S1,
			TOSS2 = fstack:popAndDrop(N, TOSS),
			{rev_delta(IP), [TOSS2|T]}
	end;
%% ) Unload fingerprint
process_instruction($), #fip{} = IP, StackStack, Space) ->
	process_instruction($(, IP, StackStack, Space);

%% q Quit
process_instruction($q, _IP, Stack, _Space) ->
	{_S2, Retval} = pop(Stack),
	{dead, Retval};



%% Handle ranges and unimplemented.

%% 0-9 Any number.
process_instruction(Instr, #fip{} = IP, Stack, _Space) when (Instr >= $0) andalso (Instr =< $9) ->
	{IP, push(Stack, Instr - $0)};
%% a-f Hexdecimal numbers.
process_instruction(Instr, #fip{} = IP, Stack, _Space) when (Instr >= $a) andalso (Instr =< $f) ->
	{IP, push(Stack, Instr - $a + 10)};

%% unimplemented
process_instruction(_Instr, #fip{} = IP, Stack, _Space) ->
	%io:format("Instruction ~c is not implemented (at x=~w y=~w).~n",
	%          [_Instr, IP#fip.x, IP#fip.y]),
	{rev_delta(IP), Stack}.


%% @spec iterate(Count, Instr, IP, Stack, Space) -> process_instr_ret()
%% @doc Iterate helper. Calls the relevant process_instruction Count times.
-spec iterate(non_neg_integer(),integer(),ip()|dead,stackstack()|integer(),fungespace()) -> process_instr_ret().
iterate(0, _Instr, IP, Stack, _Space) ->
	{IP, Stack};
% For @ and q.
iterate(_Count, _Instr, dead, Retval, _Space) ->
	{dead, Retval};
iterate(Count, Instr, IP, Stack, Space) ->
	{IP2, Stack2} = process_instruction(Instr, IP, Stack, Space),
	iterate(Count-1, Instr, IP2, Stack2, Space).
