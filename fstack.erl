-module(fstack).
%% Funge-like stack in Erlang
-export([new/0, push/2, peek/1, pop/1, popVec/1, dup/1, swap/1]).

%% Stack is a list, access at list head.
%% @type stack() = list(int()).

%% new() -> stack().
%%   Create a new stack.
new() ->
	[].

%% push(stack(), int()) -> stack().
%%   Push a value on a stack.
push([], V) when is_number(V) ->
	[V];
push(L, V) when is_list(L) andalso is_number(V) ->
	[V|L].

%% peek(stack()) -> int().
%%   Get the top value of a stack.
peek([]) ->
	0;
peek([H|_]) ->
	H.

%% pop(stack()) -> {NewStack::stack(), Value::int()}.
%%   Pop a value from a stack.
pop([]) ->
	{[], 0};
pop([H|T]) ->
	{T, H}.

%% popVec(stack()) -> {stack(), {int(), int()}}.
%%   Pop a Funge vector from a stack.
popVec([]) ->
	{[], {0, 0}};
popVec([Y]) ->
	{[], {0, Y}};
popVec([Y,X|T]) ->
	{T, {X, Y}}.

%% dup(stack()) -> stack().
%%   Duplicate the top value on a stack.
dup([]) ->
	[0, 0];
dup([H|T]) ->
	[H,H|T].

%% swap(stack()) -> stack().
%%   Swap the two top values on a stack.
swap([]) ->
	[0, 0];
swap([H1,H2|T]) ->
	[H2,H1|T];
swap([H|_]) ->
	[0,H].
