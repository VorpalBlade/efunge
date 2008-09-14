%% @doc Funge-like stack in Erlang
-module(fstack).
-include("fip.hrl").
-include("funge_types.hrl").
-export([new/0, push/2, peek/1, pop/1, popVec/1, dup/1, swap/1]).


%% @type stack() = [] | list(integer()).
%%   Stack is a list, access at list head.

%% @spec new() -> stack()
%% @doc Create a new stack.
-spec new() -> [].
new() ->
	[].

%% @spec push(stack(), integer()) -> stack()
%% @doc Push a value on a stack.
-spec push(stack(), integer()) -> stack().
push([], V) ->
	[V];
push(L, V) ->
	[V|L].

%% @spec peek(stack()) -> integer()
%% @doc Get the top value of a stack.
-spec peek(stack()) -> integer().
peek([]) ->
	0;
peek([H|_]) ->
	H.

%% @spec pop(stack()) -> {NewStack::stack(), Value::integer()}
%% @doc Pop a value from a stack.
-spec pop(stack()) -> {stack(), integer()}.
pop([]) ->
	{[], 0};
pop([H|T]) ->
	{T, H}.

%% @spec popVec(stack()) -> {stack(), {integer(), integer()}}
%% @doc Pop a Funge vector from a stack.
-spec popVec(stack()) -> {stack(), {integer(), integer()}}.
popVec([]) ->
	{[], {0, 0}};
popVec([Y]) ->
	{[], {0, Y}};
popVec([Y,X|T])->
	{T, {X, Y}}.

%% @spec dup(stack()) -> stack()
%% @doc Duplicate the top value on a stack.
-spec dup(stack()) -> stack().
dup([]) ->
	[0, 0];
dup([H|T]) ->
	[H,H|T].

%% @spec swap(stack()) -> stack()
%% @doc Swap the two top values on a stack.
-spec swap(stack()) -> stack().
swap([]) ->
	[0, 0];
swap([H1,H2|T]) ->
	[H2,H1|T];
swap([H]) ->
	[0,H].
