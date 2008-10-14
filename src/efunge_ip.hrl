%%%----------------------------------------------------------------------
%%% efunge - a Befunge-98 interpreter in Erlang.
%%% Copyright (C) 2008 Arvid Norlander <anmaster AT tele2 DOT se>
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%----------------------------------------------------------------------
-type array() :: any().

-record(fip,
	{
		x = 0 :: integer(),
		y = 0 :: integer(),
		dx = 1 :: integer(),
		dy = 0 :: integer(),
		offX = 0 :: integer(),
		offY = 0 :: integer(),
		isStringMode = false :: bool(),
		lastWasSpace = false :: bool(),
		stringBuffer = [] :: list(),
		fingerOpStacks  :: array()
	}
).

%% @type ip() = #fip{}.
%%    A Funge IP, with state.
