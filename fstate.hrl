-record(FungeState, {
	x = 0,
	y = 0,
	dx = 1,
	dy = 0,
	mode = code,
	stack = [],
	%% This will be a dict.
	space
})
