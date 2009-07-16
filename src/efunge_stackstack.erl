%%%----------------------------------------------------------------------
%%% efunge - a Befunge-98 interpreter in Erlang.
%%% Copyright (C) 2008-2008 Arvid Norlander <anmaster AT tele2 DOT se>
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
%% @doc An implementation of a Funge style stack-stack.
-module(efunge_stackstack).
-export([new/0, ss_begin/2, ss_end/2, ss_under/2]).
-export([clear/1, pop_vec_SOSS/1, push_vec_SOSS/2]).
%% Wrappers for working on TOSS. Calls functions from the efunge_stack module:
-export([push/2, peek/1, pop/1, pop_vec/1, push_vec/2]).
-export([dup/1, swap/1, pop_gnirts/1, push_list/2]).


%%====================================================================
%% Types
%%====================================================================

-include("efunge_ip.hrl").
-include("funge_types.hrl").

%% @type stack() = [] | list(integer()).
%%   Stack is a list, access at list head.
%% @type stackstack() = [] | list(stack()).
%%   Stack Stack, access at list head.


%%====================================================================
%% API - Stack wrappers for TOSS
%%====================================================================

%% @doc Push to TOSS.
%% @see efunge_stack:push/2
-spec push(stackstack(),cell()) -> stackstack().
push([TOSS|T], V) ->
	NewTOSS = efunge_stack:push(TOSS, V),
	[NewTOSS|T].
%% @doc Peek on TOSS.
%% @see efunge_stack:peek/1
-spec peek(stackstack()) -> cell().
peek([TOSS|_]) ->
	efunge_stack:peek(TOSS).
%% @doc Pop from TOSS.
%% @see efunge_stack:pop/1
-spec pop(stackstack()) -> {stackstack(),cell()}.
pop([TOSS|T]) ->
	{NewTOSS, V} = efunge_stack:pop(TOSS),
	{[NewTOSS|T], V}.
%% @doc Pop a vector from TOSS.
%% @see efunge_stack:pop_vec/1
-spec pop_vec(stackstack()) -> {stackstack(),coord()}.
pop_vec([TOSS|T]) ->
	{NewTOSS, V} = efunge_stack:pop_vec(TOSS),
	{[NewTOSS|T], V}.
%% @doc Push a vector on TOSS.
%% @see efunge_stack:push_vec/2
-spec push_vec(stackstack(), coord()) -> stackstack().
push_vec([TOSS|T], V) ->
	NewTOSS = efunge_stack:push_vec(TOSS, V),
	[NewTOSS|T].
%% @doc Duplicate the top value on TOSS.
%% @see efunge_stack:dup/1
-spec dup(stackstack()) -> stackstack().
dup([TOSS|T]) ->
	NewTOSS = efunge_stack:dup(TOSS),
	[NewTOSS|T].
%% @doc Swap the top two values on TOSS.
%% @see efunge_stack:swap/1
-spec swap(stackstack()) -> stackstack().
swap([TOSS|T]) ->
	NewTOSS = efunge_stack:swap(TOSS),
	[NewTOSS|T].
%% @doc Pop a 0"gnirts" from TOSS.
%% @see efunge_stack:pop_gnirts/1
-spec pop_gnirts(stackstack()) -> {stackstack(), [integer()]}.
pop_gnirts([TOSS|T]) ->
	{NewTOSS, V} = efunge_stack:pop_gnirts(TOSS),
	{[NewTOSS|T], V}.
%% @doc Push a list on TOSS.
%% @see efunge_stack:push_list/2
-spec push_list(stackstack(), [integer()]) -> stackstack().
push_list([TOSS|T], List) ->
	NewTOSS = efunge_stack:push_list(TOSS, List),
	[NewTOSS|T].


%% @doc Clear TOSS.
-spec clear(stackstack()) -> stackstack().
clear([_|T]) ->
	NewTOSS = efunge_stack:new(),
	[NewTOSS|T].


%%====================================================================
%% API - Stack wrappers for SOSS
%%====================================================================

%% @doc Pop a vector from SOSS. If no SOSS exists, throw 'oneStack'.
%% @see efunge_stack:pop_vec/1
-spec pop_vec_SOSS(stackstack()) -> {stackstack(),coord()}.
pop_vec_SOSS([_]) ->
	throw(oneStack);
pop_vec_SOSS([TOSS,SOSS|T]) ->
	{NewSOSS, V} = efunge_stack:pop_vec(SOSS),
	{[TOSS, NewSOSS|T], V}.
%% @doc Push a vector on SOSS. If no SOSS exists, throw 'oneStack'.
%% @see efunge_stack:push_vec/2
-spec push_vec_SOSS(stackstack(), coord()) -> stackstack().
push_vec_SOSS([_], _) ->
	throw(oneStack);
push_vec_SOSS([TOSS,SOSS|T], V) ->
	NewSOSS = efunge_stack:push_vec(SOSS, V),
	[TOSS, NewSOSS|T].


%%====================================================================
%% API - Stack Stack API
%%====================================================================

%% @spec new() -> stackstack()
%% @doc Create a new stack-stack.
-spec new() -> [[],...].
new() ->
	[[]].

%% @doc Stack-Stack Begin
-spec ss_begin(stackstack(), integer()) -> stackstack().
ss_begin(StackStack, 0) ->
	[efunge_stack:new()|StackStack];
ss_begin([OldTOSS|Tail], N) when N < 0 ->
	NewTOSS = efunge_stack:new(),
	OldTOSS1 = push_zeros(-N, OldTOSS),
	[NewTOSS, OldTOSS1|Tail];
ss_begin([OldTOSS|Tail], N) ->
	NewTOSS = efunge_stack:new(),
	{OldTOSS1, NewTOSS1} = efunge_stack:stack_to_stack(N, OldTOSS, NewTOSS),
	NewTOSS2 = lists:reverse(NewTOSS1),
	[NewTOSS2, OldTOSS1|Tail].

%% @doc Stack-Stack End
-spec ss_end(stackstack(), integer()) -> stackstack().
ss_end([_TOSS], _) ->
	throw(oneStack);
ss_end([_TOSS,SOSS|Tail], N) when N < 0 ->
	% Pop |N| items
	NewSOSS = efunge_stack:pop_drop(-N, SOSS),
	[NewSOSS|Tail];
ss_end([TOSS,SOSS|Tail], N) ->
	TempStack = efunge_stack:new(),
	{_, TempStack1} = efunge_stack:stack_to_stack(N, TOSS, TempStack),
	% Reverse the popped list and append the SOSS at the end.
	NewSOSS = lists:reverse(TempStack1, SOSS),
	[NewSOSS|Tail].

%% @doc Stack under Stack
-spec ss_under(stackstack(), integer()) -> stackstack().
ss_under([_TOSS], _) ->
	throw(oneStack);
ss_under([TOSS,SOSS|Tail], Count) when Count < 0 ->
	{NewTOSS, NewSOSS} = efunge_stack:stack_to_stack(-Count, TOSS, SOSS),
	[NewTOSS, NewSOSS|Tail];
ss_under([TOSS,SOSS|Tail], Count) ->
	{NewSOSS, NewTOSS} = efunge_stack:stack_to_stack(Count, SOSS, TOSS),
	[NewTOSS, NewSOSS|Tail].


%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Push N zeros on a stack.
-spec push_zeros(non_neg_integer(),stack()) -> stack().
push_zeros(0, Stack) ->
	Stack;
push_zeros(N, Stack) ->
	NewStack = efunge_stack:push(Stack, 0),
	push_zeros(N-1, NewStack).
