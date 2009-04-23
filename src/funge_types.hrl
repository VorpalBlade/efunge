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
-type cell() :: integer().
-type stack() :: [] | list(cell()).
-type stackstack() :: [] | list(stack()).
-type coord() :: {integer(), integer()}.
-type ip() :: #fip{}.
-type fungespace() :: atom() | tid().
-type fingerfun() :: fun((ip(), stackstack(), fungespace()) -> {ip(), stackstack()}).
-type fingerloadingfun() :: fun((ip()) -> {ok, ip()} | {error, ip()}).
-type fingerstack() :: [] | list(fingerfun()).
