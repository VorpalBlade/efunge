%%%----------------------------------------------------------------------
%%% efunge - a Befunge-98 interpreter in Erlang.
%%% Copyright (C) 2008 Arvid Norlander <anmaster AT tele2 DOT se>
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
%% @doc An implementation of a Funge style stack.
-module(efunge_stack).
-include("efunge_ip.hrl").
-include("funge_types.hrl").
-export([new/0]).
-export([push/2, peek/1, pop/1]).
-export([dup/1, swap/1]).
-export([pop_vec/1, push_vec/2]).
-export([push_list/2, pop_gnirts/1, push_gnirtses/2]).
-export([pop_drop/2, stack_to_stack/3]).

%% @type cell() = integer().
%%   A item on the stack.
%% @type stack() = [] | list(cell()).
%%   Stack is a list, access at list head.

%% @spec new() -> stack()
%% @doc Create a new stack.
-spec new() -> [].
new() ->
	[].

%% @spec push(stack(), cell()) -> stack()
%% @doc Push a value on a stack.
-spec push(stack(), cell()) -> stack_non_empty().
push(L, V) ->
	[V|L].

%% @spec peek(stack()) -> cell()
%% @doc Get the top value of a stack.
-spec peek(stack()) -> cell().
peek([]) ->
	0;
peek([H|_]) ->
	H.

%% @spec pop(stack()) -> {NewStack::stack(), Value::cell()}
%% @doc Pop a value from a stack.
-spec pop(stack()) -> {stack(), cell()}.
pop([]) ->
	{[], 0};
pop([H|T]) ->
	{T, H}.

%% @spec dup(stack()) -> stack()
%% @doc Duplicate the top value on a stack.
-spec dup(stack()) -> stack_non_empty().
dup([]) ->
	[0, 0];
dup([H|T]) ->
	[H,H|T].

%% @spec swap(stack()) -> stack()
%% @doc Swap the two top values on a stack.
-spec swap(stack()) -> stack_non_empty().
swap([]) ->
	[0, 0];
swap([H1,H2|T]) ->
	[H2,H1|T];
swap([H]) ->
	[0,H].

%% @spec pop_vec(stack()) -> {stack(), coord()}
%% @doc Pop a Funge vector from a stack. Type tagged tuples will be replaced.
-spec pop_vec(stack()) -> {stack(), coord()}.
pop_vec([]) ->
	{[], {0, 0}};
pop_vec([Y]) ->
	{[], {0, Y}};
pop_vec([Y,X|T]) ->
	{T, {X, Y}}.

%% @spec push_vec(stack(), coord()) -> stack()
%% @doc Pop a Funge vector from a stack.
-spec push_vec(stack(), coord()) -> stack_non_empty().
push_vec([], {X, Y}) ->
	[Y,X];
push_vec(S, {X, Y})->
	[Y, X|S].

%% @spec push_list(stack(), list(integer())) -> stack()
%% @doc Push a list on the stack.
-spec push_list(stack(), list(integer())) -> stack_non_empty().
push_list(Stack, []) ->
	Stack;
push_list(Stack, [H|T]) ->
	push_list([H|Stack], T).

%% @spec pop_gnirts(stack()) -> {stack(), list(integer())}
%% @doc Pop a 0gnirts.
-spec pop_gnirts(stack()) -> {stack(), list(integer())}.
pop_gnirts(Stack) ->
	pop_gnirts(Stack, []).

%% @spec pop_gnirts(stack(), list(integer())) -> {stack(), list(integer())}
%% @doc Pop a 0gnirts, internal helper for pop_gnirts/1.
-spec pop_gnirts(stack(), list()) -> {stack(), list(integer())}.
pop_gnirts([], Acc) ->
	{[], lists:reverse(Acc)};
pop_gnirts([0|T], Acc) ->
	{T, lists:reverse(Acc)};
pop_gnirts([H|T] = _Stack, Acc) ->
	pop_gnirts(T, [H|Acc]).

%% @spec push_gnirtses(stack(), list(list(integer()))) -> stack()
%% @doc Push a list of strings as a series of 0"gnirts"
-spec push_gnirtses(stack(), [[integer(),...],...]) -> stack_non_empty().
push_gnirtses(Stack, []) ->
	Stack;
push_gnirtses(Stack, [H|T]) ->
	push_gnirtses(push_list(Stack, [0|lists:reverse(H)]), T).

%% @spec pop_drop(integer(), stack()) -> stack()
%% @doc Pop N items from a stack.
-spec pop_drop(non_neg_integer(), stack()) -> stack().
pop_drop(0, Stack) ->
	Stack;
pop_drop(N, Stack) ->
	{NewStack, _} = efunge_stack:pop(Stack),
	pop_drop(N-1, NewStack).

%% @spec stack_to_stack(integer(), stack(), stack()) -> {stack(), stack()}
%% @doc This pops N elements from Stack1 and pushes them on Stack2.
%% Tail recursive. Note that order is reverse of original list.
-spec stack_to_stack(non_neg_integer(), stack(), stack()) -> {stack(), stack()}.
stack_to_stack(0, Stack1, Stack2) ->
	{Stack1, Stack2};
stack_to_stack(Count, Stack1, Stack2) ->
	{NewStack1, Item} = efunge_stack:pop(Stack1),
	NewStack2 = efunge_stack:push(Stack2, Item),
	stack_to_stack(Count-1, NewStack1, NewStack2).
