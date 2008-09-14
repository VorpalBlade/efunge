-type fungespace_table() :: integer().
-record(fspace,
	{
%%		x = undefined :: integer() | undefined,
%%		y = undefined :: integer() | undefined,
%%		maxx = undefined :: integer() | undefined,
%%		maxy = undefined :: integer() | undefined,
		space :: fungespace_table()
	}
).
-type fungespace() :: #fspace{}.
