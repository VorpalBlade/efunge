%% @doc Funge stack in Erlang
-module(fstackstack).
-include("fip.hrl").
-include("funge_types.hrl").
-export([
         new/0, ssBegin/2, ssEnd/2, ssUnder/2,
         push/2, peek/1, pop/1, popVec/1, pushVec/2, dup/1, swap/1,
         clear/1,
         popVecSOSS/1, pushVecSOSS/2
        ]).


%% @type stack() = [] | list(integer()).
%%   Stack is a list, access at list head.
%% @type stack() = [] | list(stack()).
%%   Stack Stack, access at list head.

%% Functions to work on TOSS
-spec push(stackstack(),integer()) -> stackstack().
push([TOSS|T], V) ->
	NewTOSS = fstack:push(TOSS, V),
	[NewTOSS|T].
-spec peek(stackstack()) -> integer().
peek([TOSS|_]) ->
	fstack:peek(TOSS).
-spec pop(stackstack()) -> {stackstack(),integer()}.
pop([TOSS|T]) ->
	{NewTOSS, V} = fstack:pop(TOSS),
	{[NewTOSS|T], V}.
-spec popVec(stackstack()) -> {stackstack(),{integer(),integer()}}.
popVec([TOSS|T]) ->
	{NewTOSS, V} = fstack:popVec(TOSS),
	{[NewTOSS|T], V}.
-spec pushVec(stackstack(), {integer(),integer()}) -> stackstack().
pushVec([TOSS|T], V) ->
	NewTOSS = fstack:pushVec(TOSS, V),
	[NewTOSS|T].
-spec dup(stackstack()) -> stackstack().
dup([TOSS|T]) ->
	NewTOSS = fstack:dup(TOSS),
	[NewTOSS|T].
-spec swap(stackstack()) -> stackstack().
swap([TOSS|T]) ->
	NewTOSS = fstack:swap(TOSS),
	[NewTOSS|T].
-spec clear(stackstack()) -> stackstack().
clear([_|T]) ->
	NewTOSS = fstack:new(),
	[NewTOSS|T].

%% Functions working on SOSS
-spec popVecSOSS(stackstack()) -> {stackstack(),{integer(),integer()}}.
popVecSOSS([_]) ->
	throw(oneStack);
popVecSOSS([TOSS,SOSS|T]) ->
	{NewSOSS, V} = fstack:popVec(SOSS),
	{[TOSS, NewSOSS|T], V}.
-spec pushVecSOSS(stackstack(), {integer(),integer()}) -> stackstack().
pushVecSOSS([_], _) ->
	throw(oneStack);
pushVecSOSS([TOSS,SOSS|T], V) ->
	NewSOSS = fstack:pushVec(SOSS, V),
	[TOSS, NewSOSS|T].

%% @spec new() -> stack()
%% @doc Create a new stack-stack.
-spec new() -> [[]].
new() ->
	[[]].

%% @doc Stack-Stack Begin
-spec ssBegin(stackstack(), non_neg_integer()) -> stackstack().
ssBegin(StackStack, 0) ->
	[fstack:new()|StackStack];
ssBegin([OldTOSS|Tail], N) ->
	NewTOSS = fstack:new(),
	{OldTOSS1, NewTOSS1} = stackToStack(N, OldTOSS, NewTOSS),
	NewTOSS2 = lists:reverse(NewTOSS1),
	[NewTOSS2, OldTOSS1|Tail].

%% @doc Stack-Stack End
-spec ssEnd(stackstack(), non_neg_integer()) -> stackstack().
ssEnd([_TOSS], _) ->
	throw(oneStack);
ssEnd([TOSS,SOSS|Tail], N) ->
	TempStack = fstack:new(),
	{_, TempStack1} = stackToStack(N, TOSS, TempStack),
	% Reverse the popped list and append the SOSS at the end.
	NewSOSS = lists:reverse(TempStack1, SOSS),
	[NewSOSS|Tail].

%% @doc Stack under Stack
-spec ssUnder(stackstack(), integer()) -> stackstack().
ssUnder([_TOSS], _) ->
	throw(oneStack);
ssUnder([TOSS,SOSS|Tail], Count) when Count < 0 ->
	{NewSOSS, NewTOSS} = stackToStack(-Count, SOSS, TOSS),
	[NewTOSS, NewSOSS|Tail];
ssUnder([TOSS,SOSS|Tail], Count) ->
	{NewTOSS, NewSOSS} = stackToStack(Count, TOSS, SOSS),
	[NewTOSS, NewSOSS|Tail].


%% Private functions

%% @doc This pops N elements from Stack1 to Stack2. Tail recursive.
%% Note that order is reverse of original list.
-spec stackToStack(non_neg_integer(),stack(),stack()) -> {stack(),stack()}.
stackToStack(0, Stack1, Stack2) ->
	{Stack1, Stack2};
stackToStack(Count, Stack1, Stack2) ->
	{NewStack1, Item} = fstack:pop(Stack1),
	NewStack2 = fstack:push(Stack2, Item),
	stackToStack(Count-1, NewStack1, NewStack2).
