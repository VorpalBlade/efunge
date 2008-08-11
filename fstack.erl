-module(fstack).
%% Funge-like stack in Erlang
-export([new/0, push/2, peek/1, pop/1, popVec/1, dup/1, swap/1]).

%% Stack is a list, access at list head
%% new() -> a new stack
%% push(stack, value) -> new_stack
%% peek(stack) -> value
%% pop(stack) -> {new_stack, value}
%% popVec(stack) -> {new_stack, {X, Y}}
%% dup(stack) -> new_stack
%% swap(stack) -> new_stack

new() ->
	[].

push([], V) when is_number(V) ->
	[V];
push(L, V) when is_list(L) andalso is_number(V) ->
	[V|L].

peek([]) ->
	0;
peek([H|_]) ->
	H.

pop([]) ->
	{[], 0};
pop([H|T]) ->
	{T, H}.

popVec([]) ->
	{[], {0, 0}};
popVec([Y]) ->
	{[], {0, Y}};
popVec([Y,X|T]) ->
	{T, {X, Y}}.

dup([]) ->
	[0, 0];
dup([H|T]) ->
	[H,H|T].

swap([]) ->
	[0, 0];
swap([H1,H2|T]) ->
	[H2,H1|T];
swap([H|_]) ->
	[0,H].
