%% @doc Handles y instruction.
-module(fsysinfo).
-export([sysInfo/4]).
-include("fip.hrl").
-include("fspace.hrl").
-include("funge_types.hrl").
-import(fstack, [push/2, pushVec/2]).
-define(MAX_FUNGE98, 20).


%% @doc Return value for a specific y request.
-spec pushRequest(1..20,ip(),stackstack(),fungespace(),stack()) -> stack().
% 1 Flags
pushRequest(1, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, 0);
% 2 Cell size
pushRequest(2, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, -1);
% 3 Handprint - EFUN (16#4546554E) (prime: EMUS 16#454D5553)
pushRequest(3, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, 16#4546554E);
	% I would prefer this, but would be hard to match on in a non-BIGNUM Funge.
	% "efunge - A BIGNUM Befunge-98 interpreter in Erlang"
	%push(PushStack, 16#6566756E6765202D2041204249474E554D20426566756E67652D393820696E74657270726574657220696E2045726C616E67);
% 4 Version
pushRequest(4, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, 1);
% 5 Operating paradigm
pushRequest(5, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, 0);
% 6 Path sep
pushRequest(6, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, $/);
% 7 Dimensions
pushRequest(7, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, 2);
% 8 IP ID
pushRequest(8, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, 0);
% 9 Team ID
pushRequest(9, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, 0);
% 10 IP pos
pushRequest(10, #fip{ x = X, y = Y}, _StackStack, _FungeSpace, PushStack) ->
	pushVec(PushStack, {X, Y});
% 11 IP delta
pushRequest(11, #fip{ dx = DX, dy = DY}, _StackStack, _FungeSpace, PushStack) ->
	pushVec(PushStack, {DX, DY});
% 12 Storage offset
pushRequest(12, #fip{ offX = OX, offY = OY}, _StackStack, _FungeSpace, PushStack) ->
	pushVec(PushStack, {OX, OY});
% 13 Least point
pushRequest(13, #fip{} = _IP, _StackStack, FungeSpace, PushStack) ->
	{Least, _} = fspace:getBounds(FungeSpace),
	pushVec(PushStack, Least);
% 14 Greatest point
pushRequest(14, #fip{} = _IP, _StackStack, FungeSpace, PushStack) ->
	{{Lx, Ly}, {Mx, My}} = fspace:getBounds(FungeSpace),
	pushVec(PushStack, {Mx - Lx, My - Ly});
% 15 Date
pushRequest(15, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	{{Y, M, D}, _} = erlang:universaltime(),
	push(PushStack, (Y-1900) * 256 * 256 + M * 256 + D);
% 16 Time
pushRequest(16, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	{_, {H, M, S}} = erlang:universaltime(),
	push(PushStack, H * 256 * 256 + M * 256 + S);
% 17 Stack count
pushRequest(17, #fip{} = _IP, StackStack, _FungeSpace, PushStack) ->
	push(PushStack, erlang:length(StackStack));
% 18 Sizes of stacks
pushRequest(18, #fip{} = _IP, StackStack, _FungeSpace, PushStack) ->
	pushStackLengths(StackStack, PushStack);
% 19 Cmd line args
pushRequest(19, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	Args = lists:reverse(get(efungeargs)),
	PushStack2 = push(push(PushStack, 0), 0),
	fstack:pushGnirtses(PushStack2, Args);
% 20 Environment
pushRequest(20, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, 0),
	fstack:pushGnirtses(PushStack, os:getenv()).


%% @spec sysInfo(RequestID, IP, StackStack, FungeSpace) -> stackstack()
%% @doc This implements y.
-spec sysInfo(integer(),ip(),stackstack(), fungespace()) -> stackstack().

sysInfo(RequestID, #fip{} = IP, [TOSS|T] = StackStack, FungeSpace) when RequestID =< 0 ->
	NewTOSS = pushAll(?MAX_FUNGE98, IP, StackStack, FungeSpace, TOSS),
	[NewTOSS|T];
sysInfo(RequestID, #fip{} = IP, [TOSS|T] = StackStack, FungeSpace) when RequestID < 10 ->
	NewTOSS = pushRequest(RequestID, IP, StackStack, FungeSpace, TOSS),
	[NewTOSS|T];
% Now we need to create a temp stack..
sysInfo(RequestID, #fip{} = IP, [TOSS|_] = StackStack, FungeSpace) ->
	Tmp1 = fstack:new(),
	Tmp2 = pushAll(?MAX_FUNGE98, IP, StackStack, FungeSpace, Tmp1),
	Len = erlang:length(Tmp2),
	if
		% Pick
		Len < RequestID ->
			try
				V = lists:nth(RequestID-Len, TOSS),
				fstackstack:push(StackStack, V)
			%% Handle case of too short list.
			catch
				error:function_clause -> fstackstack:push(StackStack, 0)
			end;
		true ->
			Tmp3 = fstackstack:popAndDrop(RequestID-1, Tmp2),
			{_, V} = fstack:pop(Tmp3),
			fstackstack:push(StackStack, V)
	end.

% Various helper functions

%% @doc Push all requests.
-spec pushAll(1..20,ip(),stackstack(),fungespace(),stack()) -> stack().
pushAll(0, _, _, _, PushStack) ->
	PushStack;
pushAll(RequestID, #fip{} = IP, StackStack, FungeSpace, PushStack) ->
	PushStack2 = pushRequest(RequestID, #fip{} = IP, StackStack, FungeSpace, PushStack),
	pushAll(RequestID-1, IP, StackStack, FungeSpace, PushStack2).


% Helper functions for use in pushRequest()

%% @doc Push length of all the stacks to another stack.
-spec pushStackLengths(stackstack(),stack()) -> stack().
pushStackLengths([], PushStack) ->
	PushStack;
pushStackLengths([H|T], PushStack) ->
	NewPushStack = fstack:push(PushStack, erlang:length(H)),
	pushStackLengths(T, NewPushStack).
