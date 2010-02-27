%%%----------------------------------------------------------------------
%%% efunge - a Befunge-98 interpreter in Erlang.
%%% Copyright (C) 2008-2010 Arvid Norlander <anmaster AT tele2 DOT se>
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%----------------------------------------------------------------------
%% @doc This module implements the main loop.
-module(efunge_interpreter).
-export([loop/3]).
-import(efunge_fungespace, [set/4, fetch/3]).
-import(efunge_stackstack, [push/2, pop/1, pop_vec/1, dup/1, swap/1, pop_gnirts/1, push_vec/2]).
-import(efunge_ip, [ip_forward/2, set_delta/3, set_offset/3, rev_delta/1, turn_delta_left/1, turn_delta_right/1]).

-include("efunge_ip.hrl").
-include("funge_types.hrl").
%% @headerfile "efunge_ip.hrl"

%% @spec loop(ip(), stackstack(), tid()) -> integer()
%% @doc Main loop. Do not call from anywhere but efunge:start/2!
-spec loop(ip(), stackstack(), fungespace()) -> integer().
loop(#fip{} = IP, Stack, FungeSpace) ->
	Instr = efunge_fungespace:fetch(FungeSpace, {IP#fip.x, IP#fip.y}),
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
					efunge_fungespace:delete(FungeSpace),
					Retval;
				{NewIP, NewStack} ->
					loop(ip_forward(NewIP, FungeSpace), NewStack, FungeSpace)
			end
	end.

%% @spec handle_string_mode(integer(), ip(), stackstack()) -> {ip(), stack()}
%% @doc Handle reading stuff in string mode.
-spec handle_string_mode(integer(),ip(),stackstack()) -> {ip(),stackstack()}.
handle_string_mode(Instr, #fip{ lastWasSpace = LastSpace } = IP, Stack) ->
	if
		%% This code is needed to handle SGML spaces.
		Instr =:= $\s, not LastSpace ->
			{IP#fip{ lastWasSpace=true }, push(Stack, Instr)};
		Instr =:= $\s ->
			{IP, Stack};
		Instr =:= $" ->
			{IP#fip{ isStringMode=false, lastWasSpace=false }, Stack};
		true ->
			{IP#fip{ lastWasSpace=false }, push(Stack, Instr)}
	end.

%% Finally, process instruction:

%% @doc Process an instruction.
-spec process_instruction(integer(),ip(),stackstack(), fungespace()) -> execute_return().

%%   Space
process_instruction($\s, #fip{} = IP, Stack, _Space) ->
	{IP, Stack};

%% p Put
process_instruction($p, #fip{} = IP, Stack, Space) ->
	{S1, C} = pop_vec(Stack),
	{S2, V} = pop(S1),
	set(Space, IP, C, V),
	{IP, S2};

%% g Get
process_instruction($g, #fip{} = IP, Stack, Space) ->
	{S1, C} = pop_vec(Stack),
	V = fetch(Space, IP, C),
	{IP, push(S1, V)};


%% + Add
process_instruction($+, #fip{} = IP, Stack, _Space) ->
	{S1,B} = pop(Stack), {S2,A} = pop(S1), {IP, push(S2, A + B)};
%% - Subtract
process_instruction($-, #fip{} = IP, Stack, _Space) ->
	{S1,B} = pop(Stack), {S2,A} = pop(S1), {IP, push(S2, A - B)};
%% * Multiply
process_instruction($*, #fip{} = IP, Stack, _Space) ->
	{S1,B} = pop(Stack), {S2,A} = pop(S1), {IP, push(S2, A * B)};
%% / Divide
process_instruction($/, #fip{} = IP, Stack, _Space) ->
	{S1,B} = pop(Stack),
	{S2,A} = pop(S1),
	if
		B =:= 0 -> {IP, push(S2, 0)};
		true    -> {IP, push(S2, A div B)}
	end;

%% % Remainder
process_instruction($%, #fip{} = IP, Stack, _Space) ->
	{S1,B} = pop(Stack),
	{S2,A} = pop(S1),
	if
		B =:= 0 -> {IP, push(S2, 0)};
		true    -> {IP, push(S2, A rem B)}
	end;

%% " String mode
process_instruction($", #fip{ isStringMode = StrMode } = IP, Stack, _Space) ->
	{IP#fip{ isStringMode=not StrMode }, Stack};

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
%% ? Go Away (Random direction)
process_instruction($?, #fip{} = IP, Stack, _Space) ->
	case random:uniform(4) of
		1 -> {set_delta(IP, -1,  0), Stack};
		2 -> {set_delta(IP,  1,  0), Stack};
		3 -> {set_delta(IP,  0, -1), Stack};
		4 -> {set_delta(IP,  0,  1), Stack}
	end;

%% ! Logical Not
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

%% : Duplicate
process_instruction($:, #fip{} = IP, Stack, _Space) ->
	{IP, dup(Stack)};
%% \ Swap
process_instruction($\\, #fip{} = IP, Stack, _Space) ->
	{IP, swap(Stack)};
%% $ Pop
process_instruction($$, #fip{} = IP, Stack, _Space) ->
	{S1, _} = pop(Stack),
	{IP, S1};

%% # Trampoline
process_instruction($#, #fip{} = IP, Stack, Space) ->
	{ip_forward(IP, Space), Stack};

%% _ North-South If
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

%% , Output Character
process_instruction($, , #fip{} = IP, Stack, _Space) ->
	{NewStack, Val} = pop(Stack),
	try
		io:put_chars([abs(Val)]),
		{IP, NewStack}
	catch
		error:badarg ->
			{rev_delta(IP), NewStack}
	end;

%% . Output Integer
process_instruction($., #fip{} = IP, Stack, _Space) ->
	{NewStack, Val} = pop(Stack),
	io:format("~w ", [Val]),
	{IP, NewStack};

%% ~ Input Character
process_instruction($~, #fip{} = IP, Stack, _Space) ->
	case efunge_input:read_char() of
		eof    -> {rev_delta(IP), Stack};
		error  -> {rev_delta(IP), Stack};
		Result -> {IP, push(Stack, Result)}
	end;
%% & Input Integer
process_instruction($&, #fip{} = IP, Stack, _Space) ->
	case efunge_input:read_integer() of
		eof    -> {rev_delta(IP), Stack};
		error  -> {rev_delta(IP), Stack};
		Result -> {IP, push(Stack, Result)}
	end;

%% @ Stop
process_instruction($@, #fip{} = _IP, _Stack, _Space) ->
	{dead, 0};


%% Begin Funge-98 instructions.

%% [ Turn Left
process_instruction($[, #fip{} = IP, Stack, _Space) ->
	{turn_delta_left(IP), Stack};
%% ] Turn Right
process_instruction($], #fip{} = IP, Stack, _Space) ->
	{turn_delta_right(IP), Stack};

%% ; Jump Over
process_instruction($;, #fip{} = IP, Stack, Space) ->
	{efunge_ip:find_next_match(ip_forward(IP, Space), $;, Space), Stack};

%% k Iterate
process_instruction($k, #fip{} = IP, Stack, Space) ->
	{S1, Count} = pop(Stack),
	if
		Count < 0 ->
			{rev_delta(IP), S1};
		Count =:= 0 ->
			{IP2, _} = efunge_ip:find_next_non_space(ip_forward(IP, Space), Space),
			{IP2, S1};
		true ->
			{InstrPos, Instr} = efunge_ip:find_next_non_space(ip_forward(IP, Space), Space),
			if
				%% This is actually buggy, we somehow need to keep track of
				%% position here and check after each iteration.
				Instr =:= $k ->
					iterate(Count, Instr, InstrPos, S1, Space);
				true ->
					iterate(Count, Instr, IP, S1, Space)
			end
	end;

%% ' Fetch Character
process_instruction($', #fip{} = IP, Stack, Space) ->
	#fip{ x = X, y = Y} = NewIP = ip_forward(IP, Space),
	Value = efunge_fungespace:fetch(Space, {X, Y}),
	{NewIP, push(Stack, Value)};

%% s Store Character
process_instruction($s, #fip{} = IP, Stack, Space) ->
	#fip{ x = X, y = Y} = NewIP = ip_forward(IP, Space),
	{S1, Value} = pop(Stack),
	efunge_fungespace:set(Space, {X, Y}, Value),
	{NewIP, S1};

%% n Clear Stack
process_instruction($n, #fip{} = IP, Stack, _Space) ->
	{IP, efunge_stackstack:clear(Stack)};

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

%% x Absolute Delta
process_instruction($x, #fip{} = IP, Stack, _Space) ->
	{S1, {X, Y}} = pop_vec(Stack),
	{set_delta(IP, X,  Y), S1};

%% j Jump Forward
process_instruction($j, #fip{} = IP, Stack, Space) ->
	{S1, Dist} = pop(Stack),
	{efunge_ip:jump(IP, Space, Dist), S1};


%% r Reflect
process_instruction($r, #fip{} = IP, Stack, _Space) ->
	{rev_delta(IP), Stack};
%% z No Operation
process_instruction($z, #fip{} = IP, Stack, _Space) ->
	{IP, Stack};

%% { Begin Block
process_instruction(${, #fip{ x = X, y = Y, dx = DX, dy = DY, offX = OX, offY = OY} = IP, StackStack, _Space) ->
	{S1, N} = pop(StackStack),
	S2 = efunge_stackstack:ss_begin(S1, N),
	S3 = efunge_stackstack:push_vec_SOSS(S2, {OX, OY}),
	IP2 = set_offset(IP, X+DX, Y+DY),
	{IP2, S3};

%% } End Block
process_instruction($}, #fip{} = IP, StackStack, _Space) ->
	{S1, N} = pop(StackStack),
	try
		{S2, {OX, OY}} = efunge_stackstack:pop_vec_SOSS(S1),
		S3 = efunge_stackstack:ss_end(S2, N),
		IP2 = set_offset(IP, OX, OY),
		{IP2, S3}
	catch
		throw:oneStack -> {rev_delta(IP), S1}
	end;

%% u Stack Under Stack
process_instruction($u, #fip{} = IP, StackStack, _Space) ->
	{S1, Count} = pop(StackStack),
	try
		S2 = efunge_stackstack:ss_under(S1, Count),
		{IP, S2}
	catch
		throw:oneStack -> {rev_delta(IP), S1}
	end;

%% y Get SysInfo
process_instruction($y, #fip{} = IP, Stack, Space) ->
	{S1, N} = pop(Stack),
	{IP, efunge_sysinfo:system_info(N, IP, S1, Space)};

%% i Input File
process_instruction($i, #fip{} = IP, Stack, Space) ->
	{S1, Filename} = pop_gnirts(Stack),
	{S2, Flags} = pop(S1),
	{S3, Vector} = pop_vec(S2),
	IsBinaryMode = (Flags band 1) =:= 1,
	Retval = efunge_fungespace:load(Space, IP, Filename, IsBinaryMode, Vector),
	case Retval of
		error -> {rev_delta(IP), S3};
		_     -> {IP, push_vec(push_vec(S3, Retval), Vector)}
	end;

%% o Output file
process_instruction($o, #fip{} = IP, Stack, Space) ->
	{S1, Filename} = pop_gnirts(Stack),
	{S2, Flags} = pop(S1),
	{S3, Offset} = pop_vec(S2),
	{S4, Size} = pop_vec(S3),
	IsTextFile = (Flags band 1) =:= 1,
	Retval = efunge_fungespace:save(Space, IP, Filename, IsTextFile, Offset, Size),
	case Retval of
		error -> {rev_delta(IP), S4};
		ok    -> {IP, S4}
	end;


%% ( Load Semantics
process_instruction($(, #fip{} = IP, StackStack, _Space) ->
	{S1, N} = pop(StackStack),
	if
		N < 0 -> {rev_delta(IP), S1};
		true ->
			{S2, Fingerprint} = build_fingerprint(N, S1, 0),
			case efunge_fingermanager:load(IP, Fingerprint) of
				{error, _} ->
					{efunge_ip:rev_delta(IP), S2};
				{ok, IP2} ->
					S3 = push(S2, Fingerprint),
					S4 = push(S3, 1),
					{IP2, S4}
			end
	end;
%% ) Unload Semantics
process_instruction($), #fip{} = IP, StackStack, _Space) ->
	{S1, N} = pop(StackStack),
	if
		N < 0 -> {rev_delta(IP), S1};
		true ->
			{S2, Fingerprint} = build_fingerprint(N, S1, 0),
			#fip{} = IP2 = efunge_fingermanager:unload(IP, Fingerprint),
			{IP2, S2}
	end;

%% q Quit
process_instruction($q, #fip{} = _IP, Stack, _Space) ->
	{_S2, Retval} = pop(Stack),
	{dead, Retval};



%% Handle ranges and unimplemented.

%% 0-9 Any number.
process_instruction(Instr, #fip{} = IP, Stack, _Space) when Instr >= $0, Instr =< $9 ->
	{IP, push(Stack, Instr - $0)};
%% a-f Hexdecimal numbers.
process_instruction(Instr, #fip{} = IP, Stack, _Space) when Instr >= $a, Instr =< $f ->
	{IP, push(Stack, Instr - $a + 10)};
%% A-Z Fingerprints.
process_instruction(Instr, #fip{} = IP, Stack, Space) when Instr >= $A, Instr =< $Z ->
	efunge_fingermanager:execute(Instr, IP, Stack, Space);

%% unimplemented
process_instruction(_Instr, #fip{} = IP, Stack, _Space) ->
	%io:format("Instruction ~c is not implemented (at x=~w y=~w).~n",
	%          [_Instr, IP#fip.x, IP#fip.y]),
	{rev_delta(IP), Stack}.


%% @spec iterate(Count, Instr, IP, Stack, Space) -> execute_return()
%% @doc Iterate helper. Calls the relevant process_instruction Count times.
-spec iterate(non_neg_integer(),integer(),ip()|dead,stackstack()|integer(),fungespace()) -> execute_return().
iterate(0, _Instr, IP, Stack, _Space) ->
	{IP, Stack};
%% For @ and q.
iterate(_Count, _Instr, dead, Retval, _Space) ->
	{dead, Retval};
%% Should insert cases for k here.
iterate(Count, Instr, IP, Stack, Space) ->
	{IP2, Stack2} = process_instruction(Instr, IP, Stack, Space),
	iterate(Count-1, Instr, IP2, Stack2, Space).


%% @doc Build a fingerprint.
-spec build_fingerprint(non_neg_integer(),stackstack(), integer()) -> {stackstack(), integer()}.
build_fingerprint(0, Stack, Result) ->
	{Stack, Result};
build_fingerprint(Count, Stack, Result) ->
	{S2, Val} = pop(Stack),
	R2 = Result bsl 8,
	build_fingerprint(Count-1, S2, R2 + Val).
