-record(fip,
	{
		x = 0 :: integer(),
		y = 0 :: integer(),
		dx = 1 :: integer(),
		dy = 0 :: integer(),
		isStringMode = false :: bool(),
		lastWasSpace = false :: bool(),
		stringBuffer = [] :: list()
	}
).
