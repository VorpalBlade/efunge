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
%% @doc NULL fingerprint.
-module(fingNULL).
-include("../efunge_ip.hrl").
-include("../funge_types.hrl").
-export([load/1]).


%% @doc Load the NULL fingerprint.
-spec load(ip()) -> {ok, ip()}.
load(IP) ->
	IP2 = load_ops(IP, lists:seq($A, $Z)),
	{ok, IP2}.


%% Private funtions

%% Tail recursive load.

%% @doc Load NULL.
-spec load_ops(ip(),string()) -> ip().
load_ops(IP, []) ->
	IP;
load_ops(IP, [H|T]) ->
	IP2 = efunge_fingermanager:push_fun(H, IP, fun efunge_fingerstack:reflect/3),
	load_ops(IP2, T).
