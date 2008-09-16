%% @doc Funge stack-stack in Erlang
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
%% @type stackstack() = [] | list(stack()).
%%   Stack Stack, access at list head.

%% Functions to work on TOSS

%% @doc Push to TOSS.
%% @see fstack:push/2
-spec push(stackstack(),integer()) -> stackstack().
push([TOSS|T], V) ->
	NewTOSS = fstack:push(TOSS, V),
	[NewTOSS|T].
%% @doc Peek on TOSS.
%% @see fstack:peek/1
-spec peek(stackstack()) -> integer().
peek([TOSS|_]) ->
	fstack:peek(TOSS).
%% @doc Pop from TOSS.
%% @see fstack:pop/1
-spec pop(stackstack()) -> {stackstack(),integer()}.
pop([TOSS|T]) ->
	{NewTOSS, V} = fstack:pop(TOSS),
	{[NewTOSS|T], V}.
%% @doc Pop a vector from TOSS.
%% @see fstack:popVec/1
-spec popVec(stackstack()) -> {stackstack(),coord()}.
popVec([TOSS|T]) ->
	{NewTOSS, V} = fstack:popVec(TOSS),
	{[NewTOSS|T], V}.
%% @doc Push a vector on TOSS.
%% @see fstack:pushVec/2
-spec pushVec(stackstack(), coord()) -> stackstack().
pushVec([TOSS|T], V) ->
	NewTOSS = fstack:pushVec(TOSS, V),
	[NewTOSS|T].
%% @doc Duplicate the top value on TOSS.
%% @see fstack:dup/1
-spec dup(stackstack()) -> stackstack().
dup([TOSS|T]) ->
	NewTOSS = fstack:dup(TOSS),
	[NewTOSS|T].
%% @doc Swap the top two values on TOSS.
%% @see fstack:swap/1
-spec swap(stackstack()) -> stackstack().
swap([TOSS|T]) ->
	NewTOSS = fstack:swap(TOSS),
	[NewTOSS|T].
%% @doc Clear TOSS.
-spec clear(stackstack()) -> stackstack().
clear([_|T]) ->
	NewTOSS = fstack:new(),
	[NewTOSS|T].

%% Functions working on SOSS

%% @doc Pop a vector from SOSS. If no SOSS exists, throw 'oneStack'.
%% @see fstack:popVec/1
-spec popVecSOSS(stackstack()) -> {stackstack(),coord()}.
popVecSOSS([_]) ->
	throw(oneStack);
popVecSOSS([TOSS,SOSS|T]) ->
	{NewSOSS, V} = fstack:popVec(SOSS),
	{[TOSS, NewSOSS|T], V}.
%% @doc Push a vector on SOSS. If no SOSS exists, throw 'oneStack'.
%% @see fstack:pushVec/2
-spec pushVecSOSS(stackstack(), coord()) -> stackstack().
pushVecSOSS([_], _) ->
	throw(oneStack);
pushVecSOSS([TOSS,SOSS|T], V) ->
	NewSOSS = fstack:pushVec(SOSS, V),
	[TOSS, NewSOSS|T].

%% @spec new() -> stackstack()
%% @doc Create a new stack-stack.
-spec new() -> [[]].
new() ->
	[[]].

%% @doc Stack-Stack Begin
-spec ssBegin(stackstack(), non_neg_integer()) -> stackstack().
ssBegin(StackStack, 0) ->
	[fstack:new()|StackStack];
ssBegin([OldTOSS|Tail], N) when N < 0 ->
	NewTOSS = fstack:new(),
	OldTOSS1 = pushNZero(-N, OldTOSS),
	[NewTOSS, OldTOSS1|Tail];
ssBegin([OldTOSS|Tail], N) ->
	NewTOSS = fstack:new(),
	{OldTOSS1, NewTOSS1} = fstack:stack_to_stack(N, OldTOSS, NewTOSS),
	NewTOSS2 = lists:reverse(NewTOSS1),
	[NewTOSS2, OldTOSS1|Tail].

%% @doc Stack-Stack End
-spec ssEnd(stackstack(), non_neg_integer()) -> stackstack().
ssEnd([_TOSS], _) ->
	throw(oneStack);
ssEnd([_TOSS,SOSS|Tail], N) when N < 0 ->
	% Pop |N| items
	NewSOSS = fstack:popAndDrop(-N, SOSS),
	[NewSOSS|Tail];
ssEnd([TOSS,SOSS|Tail], N) ->
	TempStack = fstack:new(),
	{_, TempStack1} = fstack:stack_to_stack(N, TOSS, TempStack),
	% Reverse the popped list and append the SOSS at the end.
	NewSOSS = lists:reverse(TempStack1, SOSS),
	[NewSOSS|Tail].

%% @doc Stack under Stack
-spec ssUnder(stackstack(), integer()) -> stackstack().
ssUnder([_TOSS], _) ->
	throw(oneStack);
ssUnder([TOSS,SOSS|Tail], Count) when Count < 0 ->
	{NewTOSS, NewSOSS} = fstack:stack_to_stack(-Count, TOSS, SOSS),
	[NewTOSS, NewSOSS|Tail];
ssUnder([TOSS,SOSS|Tail], Count) ->
	{NewSOSS, NewTOSS} = fstack:stack_to_stack(Count, SOSS, TOSS),
	[NewTOSS, NewSOSS|Tail].

%% Private functions

%% @doc Push N zeros on a stack.
-spec pushNZero(non_neg_integer(),stack()) -> stack().
pushNZero(0, Stack) ->
	Stack;
pushNZero(N, Stack) ->
	NewStack = fstack:push(Stack, 0),
	pushNZero(N-1, NewStack).
