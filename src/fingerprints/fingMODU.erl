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

-import(fstackstack, [push/2, pop/1]).


modu_signed(IP, Stack, _Space) ->
	{S2, Y} = pop(Stack),
	{S3, X} = pop(S2),
	if
		Y =:= 0 -> {IP, push(S2, 0)};
		true    -> {IP, push(S2, X - floordiv(X, Y) * Y)}
	end.

modu_c99(IP, Stack, _Space) ->
	{S2, Y} = pop(Stack),
	{S3, X} = pop(S2),
	if
		Y =:= 0 -> {IP, push(S2, 0)};
		%% Note: I don't know if this is well defined in Erlang. If not please
		%% contact me.
		true    -> {IP, push(S2, X rem Y)}
	end.

modu_unsigned(IP, Stack, _Space) ->
	{S2, Y} = pop(Stack),
	{S3, X} = pop(S2),
	if
		Y =:= 0 -> {IP, push(S2, 0)};
		true    -> {IP, push(S2, abs(X rem Y))}
	end.


% M	signedResult	Signed result
% R	remainder	C style reminder
% U	unsignedResult	Unsigned result

%% @doc Load the MODU fingerprint.
-spec load(ip()) -> {ok, ip()}.
load(IP) ->
	IP2 = ffingermanager:push_funs(IP,
		[{$M, fun ?MODULE:modu_signed/3},
		 {$R, fun ?MODULE:modu_c99/3},
		 {$U, fun ?MODULE:modu_unsigned/3}]),
	{ok, IP2}.

%% Private funtions

floordiv(X, Y) ->
	R = X div Y,
	if
		R < 0 -> R-1;
		true   -> R
	end.
