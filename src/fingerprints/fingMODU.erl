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
%% @doc MODU fingerprint.
-module(fingMODU).
-include("../fip.hrl").
-include("../funge_types.hrl").
-export([load/1]).
%% The implemented functions
-export([modu_signed/3, modu_c99/3, modu_unsigned/3]).

%% Import common functions:
-import(fstackstack, [push/2, pop_int/1]).


%% @doc Load the MODU fingerprint.
-spec load(ip()) -> {ok, ip()}.
load(IP) ->
	IP2 = ffingermanager:push_funs(IP,
		[{$M, fun ?MODULE:modu_signed/3},
		 {$R, fun ?MODULE:modu_c99/3},
		 {$U, fun ?MODULE:modu_unsigned/3}]),
	{ok, IP2}.


%% The fingerprint functions

-spec modu_signed(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
modu_signed(IP, Stack, _Space) ->
	{S2, Y} = pop_int(Stack),
	{S3, X} = pop_int(S2),
	if
		Y =:= 0 -> {IP, push(S3, 0)};
		true    -> {IP, push(S3, X - floordiv(X, Y) * Y)}
	end.

-spec modu_c99(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
modu_c99(IP, Stack, _Space) ->
	{S2, Y} = pop_int(Stack),
	{S3, X} = pop_int(S2),
	if
		Y =:= 0 -> {IP, push(S3, 0)};
		%% FIXME: Note: I don't know if this is well defined in Erlang.
		true    -> {IP, push(S3, X rem Y)}
	end.

-spec modu_unsigned(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
modu_unsigned(IP, Stack, _Space) ->
	{S2, Y} = pop_int(Stack),
	{S3, X} = pop_int(S2),
	if
		Y =:= 0 -> {IP, push(S3, 0)};
		true    -> {IP, push(S3, abs(X rem Y))}
	end.


%% Private funtions
-spec floordiv(integer(),integer()) -> integer().
floordiv(X, Y) ->
	R = X div Y,
	if
		R < 0 -> R-1;
		true   -> R
	end.
