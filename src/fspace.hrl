-type fungespace_table() :: integer().
-record(fungespace,
	{
		x :: integer(),
		y :: integer(),
		maxx :: integer(),
		maxy :: integer(),
		space :: fungespace_table()
	}
).
-type fungespace() :: #fungespace{}.
