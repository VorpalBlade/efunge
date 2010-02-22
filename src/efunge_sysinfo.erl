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
%% @private For use in the efunge_interpreter module only.
%% @doc This module handles y instruction.
-module(efunge_sysinfo).
-export([system_info/4]).

-import(efunge_stack, [push/2, push_vec/2]).
-define(MAX_FUNGE98, 20).

-include("efunge_ip.hrl").
-include("funge_types.hrl").
%% @headerfile "efunge_ip.hrl"

%% @doc Return value for a specific y request.
-spec push_request(1..20,ip(),stackstack(),fungespace(),stack()) -> stack().
%% 1 Flags
push_request(1, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, 2#110);
%% 2 Cell size
push_request(2, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, -1);
%% 3 Handprint - EFUN (16#4546554E)
push_request(3, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, 16#4546554E);
	%% I would prefer this, but would be hard to match on in a non-BIGNUM Funge.
	%% "efunge - A BIGNUM Befunge-98 interpreter in Erlang"
	%%push(PushStack, 16#6566756E6765202D2041204249474E554D20426566756E67652D393820696E74657270726574657220696E2045726C616E67);
%% 4 Version
push_request(4, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, 1);
%% 5 Operating paradigm
push_request(5, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, 0);
%% 6 Path sep
push_request(6, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, $/);
%% 7 Dimensions
push_request(7, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, 2);
%% 8 IP ID
push_request(8, #fip{ipID=ID} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, ID);
%% 9 Team ID
push_request(9, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, 0);
%% 10 IP pos
push_request(10, #fip{ x = X, y = Y}, _StackStack, _FungeSpace, PushStack) ->
	push_vec(PushStack, {X, Y});
%% 11 IP delta
push_request(11, #fip{ dx = DX, dy = DY}, _StackStack, _FungeSpace, PushStack) ->
	push_vec(PushStack, {DX, DY});
%% 12 Storage offset
push_request(12, #fip{ offX = OX, offY = OY}, _StackStack, _FungeSpace, PushStack) ->
	push_vec(PushStack, {OX, OY});
%% 13 Least point
push_request(13, #fip{} = _IP, _StackStack, FungeSpace, PushStack) ->
	{Least, _} = efunge_fungespace:get_bounds_thread_exact(FungeSpace),
	push_vec(PushStack, Least);
%% 14 Greatest point
push_request(14, #fip{} = _IP, _StackStack, FungeSpace, PushStack) ->
	{{Lx, Ly}, {Mx, My}} = efunge_fungespace:get_bounds_thread_exact(FungeSpace),
	push_vec(PushStack, {Mx - Lx, My - Ly});
%% 15 Date
push_request(15, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	{{Y, M, D}, _} = erlang:universaltime(),
	push(PushStack, (Y-1900) * 256 * 256 + M * 256 + D);
%% 16 Time
push_request(16, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	{_, {H, M, S}} = erlang:universaltime(),
	push(PushStack, H * 256 * 256 + M * 256 + S);
%% 17 Stack count
push_request(17, #fip{} = _IP, StackStack, _FungeSpace, PushStack) ->
	push(PushStack, erlang:length(StackStack));
%% 18 Sizes of stacks
push_request(18, #fip{} = _IP, StackStack, _FungeSpace, PushStack) ->
	push_stack_lengths(StackStack, PushStack);
%% 19 Cmd line args
push_request(19, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	Args = lists:reverse(efunge_global_data:get_cmdline()),
	PushStack2 = push(push(PushStack, 0), 0),
	efunge_stack:push_gnirtses(PushStack2, Args);
%% 20 Environment
push_request(20, #fip{} = _IP, _StackStack, _FungeSpace, PushStack) ->
	push(PushStack, 0),
	efunge_stack:push_gnirtses(PushStack, os:getenv()).


%% @spec system_info(RequestID, IP, StackStack, FungeSpace) -> stackstack()
%% @doc This implements y.
-spec system_info(integer(),ip(),stackstack(), fungespace()) -> stackstack().

system_info(RequestID, #fip{} = IP, [TOSS|T] = StackStack, FungeSpace) when RequestID =< 0 ->
	NewTOSS = push_all(?MAX_FUNGE98, IP, StackStack, FungeSpace, TOSS),
	[NewTOSS|T];
system_info(RequestID, #fip{} = IP, [TOSS|T] = StackStack, FungeSpace) when RequestID < 10 ->
	NewTOSS = push_request(RequestID, IP, StackStack, FungeSpace, TOSS),
	[NewTOSS|T];
%% Now we need to create a temp stack..
system_info(RequestID, #fip{} = IP, [TOSS|_] = StackStack, FungeSpace) ->
	Tmp1 = efunge_stack:new(),
	Tmp2 = push_all(?MAX_FUNGE98, IP, StackStack, FungeSpace, Tmp1),
	Len = erlang:length(Tmp2),
	if
		%% Pick
		Len < RequestID ->
			try
				V = lists:nth(RequestID-Len, TOSS),
				efunge_stackstack:push(StackStack, V)
			%% Handle case of too short list.
			catch
				error:function_clause -> efunge_stackstack:push(StackStack, 0)
			end;
		%% Normal, get it from the temp stack.
		true ->
			Tmp3 = efunge_stack:pop_drop(RequestID-1, Tmp2),
			{_, V} = efunge_stack:pop(Tmp3),
			efunge_stackstack:push(StackStack, V)
	end.

%% Various helper functions

%% @doc Push all requests.
-spec push_all(0..20,ip(),stackstack(),fungespace(),stack()) -> stack().
push_all(0, _IP, _StackStack, _FungeSpace, PushStack) ->
	PushStack;
push_all(RequestID, #fip{} = IP, StackStack, FungeSpace, PushStack) ->
	PushStack2 = push_request(RequestID, #fip{} = IP, StackStack, FungeSpace, PushStack),
	push_all(RequestID-1, IP, StackStack, FungeSpace, PushStack2).


%% Helper functions for use in push_request()

%% @doc Push length of all the stacks to another stack.
-spec push_stack_lengths(stackstack(),stack()) -> stack().
push_stack_lengths([], PushStack) ->
	PushStack;
push_stack_lengths([H|T], PushStack) ->
	NewPushStack = efunge_stack:push(PushStack, erlang:length(H)),
	push_stack_lengths(T, NewPushStack).
