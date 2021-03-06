%%%----------------------------------------------------------------------
%%% efunge - a Befunge-98 interpreter in Erlang.
%%% Copyright (C) 2008-2010 Arvid Norlander <anmaster AT tele2 DOT se>
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
-type stack() :: [cell()].
-type stack_non_empty() :: [cell(),...].
-type stackstack() :: [stack(),...].
-type coord() :: {cell(), cell()}.
-type rect() :: {coord(), coord()}.
-type ip() :: #fip{}.
-type fungespace() :: atom() | ets:tid().

-type return_exit() :: {dead, integer()}.
-type return_normal() :: {ip(), stackstack()}.
-type execute_return() :: return_normal() | return_exit().

-type fingerfun() :: fun((ip(), stackstack(), fungespace()) -> execute_return()).
-type fingerloadingfun() :: fun((ip()) -> {ok, ip()} | {error, ip()}).
-type fingerstack() :: [] | list(fingerfun()).
