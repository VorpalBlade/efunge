%% @doc Funge-like stack in Erlang
-module(fstack).
-export([new/0, push/2, peek/1, pop/1, popVec/1, dup/1, swap/1]).


%% @type stack() = list(int()).
%%   Stack is a list, access at list head.

%% @spec new() -> stack()
%% @doc Create a new stack.
new() ->
	[].

%% @spec push(stack(), int()) -> stack()
%% @doc Push a value on a stack.
push([], V) ->
	[V];
push(L, V) ->
	[V|L].

%% @spec peek(stack()) -> int()
%% @doc Get the top value of a stack.
peek([]) ->
	0;
peek([H|_]) ->
	H.

%% @spec pop(stack()) -> {NewStack::stack(), Value::int()}
%% @doc Pop a value from a stack.
pop([]) ->
	{[], 0};
pop([H|T]) ->
	{T, H}.

%% @spec popVec(stack()) -> {stack(), {int(), int()}}
%% @doc Pop a Funge vector from a stack.
popVec([]) ->
	{[], {0, 0}};
popVec([Y]) ->
	{[], {0, Y}};
popVec([Y,X|T]) ->
	{T, {X, Y}}.

%% @spec dup(stack()) -> stack()
%% @doc Duplicate the top value on a stack.
dup([]) ->
	[0, 0];
dup([H|T]) ->
	[H,H|T].

%% @spec swap(stack()) -> stack()
%% @doc Swap the two top values on a stack.
swap([]) ->
	[0, 0];
swap([H1,H2|T]) ->
	[H2,H1|T];
swap([H|_]) ->
	[0,H].
