-type stack_items() :: integer() | {float, float()}.
-type stack() :: [] | [stack_items()].
-type coord() :: {integer(), integer()}.
-type fungespace() :: integer().
-type state() :: #fip{}.