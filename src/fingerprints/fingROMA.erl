%%%----------------------------------------------------------------------
%%% efunge - a Befunge-98 interpreter in Erlang.
%%% Copyright (C) 2008-2009 Arvid Norlander <anmaster AT tele2 DOT se>
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
%% @doc ROMA fingerprint.
-module(fingROMA).
-include("../efunge_ip.hrl").
-include("../funge_types.hrl").
-export([load/1]).


%% @doc Load the ROMA fingerprint.
-spec load(ip()) -> {ok, ip()}.
load(IP) ->
	IP2 = load_ops(IP,
		[{$C, 100},
		 {$D, 500},
		 {$I, 1},
		 {$L, 50},
		 {$M, 1000},
		 {$V, 5},
		 {$X, 10}]),
	{ok, IP2}.


%% Private funtions

%% @doc Return a fingerprint fun that push Amount.
-spec make_pusher(integer()) -> fun((ip(),stackstack(),fungespace()) -> {ip(),stackstack()}).
make_pusher(Amount) ->
	fun(IP, Stack, _Space) ->
		{IP, efunge_stackstack:push(Stack, Amount)}
	end.

%% @doc Load functions, constructed using make_pusher/1
-spec load_ops(ip(),[{char(), integer()}]) -> ip().
load_ops(IP, []) ->
	IP;
load_ops(IP, [{Instr,Amount}|T]) ->
	IP2 = efunge_fingermanager:push_fun(Instr, IP, make_pusher(Amount)),
	load_ops(IP2, T).
