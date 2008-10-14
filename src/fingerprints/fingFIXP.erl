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
%% @doc FIXP fingerprint.
-module(fingFIXP).
-include("../efunge_ip.hrl").
-include("../funge_types.hrl").
-export([load/1]).
%% The implemented functions
-export([fixp_and/3,
         fixp_acos/3,
         fixp_cos/3,
         fixp_rand/3,
         fixp_sin/3,
         fixp_asin/3,
         fixp_neg/3,
         fixp_or/3,
         fixp_mulpi/3,
         fixp_sqrt/3,
         fixp_pow/3,
         fixp_signbit/3,
         fixp_tan/3,
         fixp_atan/3,
         fixp_abs/3,
         fixp_xor/3]).

%% Import common functions:
-import(efunge_stackstack, [push/2, pop/1]).

-define(PI, 3.14159265358979323846).
-define(F_PI_DIV_180, (?PI / 180)).
-define(F_180_DIV_PI, (180 / ?PI)).

%% @doc Load the FIXP fingerprint.
-spec load(ip()) -> {ok, ip()}.
load(IP) ->
	IP2 = efunge_fingermanager:push_funs(IP, [
		{$A, fun ?MODULE:fixp_and/3},
		{$B, fun ?MODULE:fixp_acos/3},
		{$C, fun ?MODULE:fixp_cos/3},
		{$D, fun ?MODULE:fixp_rand/3},
		{$I, fun ?MODULE:fixp_sin/3},
		{$J, fun ?MODULE:fixp_asin/3},
		{$N, fun ?MODULE:fixp_neg/3},
		{$O, fun ?MODULE:fixp_or/3},
		{$P, fun ?MODULE:fixp_mulpi/3},
		{$Q, fun ?MODULE:fixp_sqrt/3},
		{$R, fun ?MODULE:fixp_pow/3},
		{$S, fun ?MODULE:fixp_signbit/3},
		{$T, fun ?MODULE:fixp_tan/3},
		{$U, fun ?MODULE:fixp_atan/3},
		{$V, fun ?MODULE:fixp_abs/3},
		{$X, fun ?MODULE:fixp_xor/3}]),
	{ok, IP2}.


%% The fingerprint functions

%% @spec fixp_and(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc A - bitwise and
-spec fixp_and(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
fixp_and(IP, Stack, _Space) ->
	{S1, B} = pop(Stack),
	{S2, A} = pop(S1),
	{IP, push(S2, A band B)}.

%% @spec fixp_acos(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc B - arccos
-spec fixp_acos(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
fixp_acos(IP, Stack, _Space) ->
	{S1, N} = pop(Stack),
	try
		R = 10000 * math:acos(N / 10000) * ?F_180_DIV_PI,
		{IP, push(S1, round(R))}
	catch
		error:badarith ->
			{IP, push(S1, 0)}
	end.

%% @spec fixp_cos(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc C - cos
-spec fixp_cos(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
fixp_cos(IP, Stack, _Space) ->
	{S1, N} = pop(Stack),
	R = 10000 * math:cos((N / 10000) * ?F_PI_DIV_180),
	{IP, push(S1, round(R))}.

%% @spec fixp_rand(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc D - random number
-spec fixp_rand(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
fixp_rand(IP, Stack, _Space) ->
	{S1, N} = pop(Stack),
	if
		N =< 0 -> {efunge_ip:rev_delta(IP), S1};
		true   -> {IP, push(S1, random:uniform(N))}
	end.

%% @spec fixp_sin(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc I - sin
-spec fixp_sin(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
fixp_sin(IP, Stack, _Space) ->
	{S1, N} = pop(Stack),
	R = 10000 * math:sin((N / 10000) * ?F_PI_DIV_180),
	{IP, push(S1, round(R))}.

%% @spec fixp_asin(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc J - arcsin
-spec fixp_asin(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
fixp_asin(IP, Stack, _Space) ->
	{S1, N} = pop(Stack),
	try
		R = 10000 * math:asin(N / 10000) * ?F_180_DIV_PI,
		{IP, push(S1, round(R))}
	catch
		error:badarith ->
			{IP, push(S1, 0)}
	end.

%% @spec fixp_neg(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc N - negate
-spec fixp_neg(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
fixp_neg(IP, Stack, _Space) ->
	{S1, N} = pop(Stack),
	{IP, push(S1, -N)}.

%% @spec fixp_or(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc O - bitwise or
-spec fixp_or(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
fixp_or(IP, Stack, _Space) ->
	{S1, B} = pop(Stack),
	{S2, A} = pop(S1),
	{IP, push(S2, A bor B)}.

%% @spec fixp_mulpi(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc P - multiply by pi
-spec fixp_mulpi(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
fixp_mulpi(IP, Stack, _Space) ->
	{S1, N} = pop(Stack),
	{IP, push(S1, round(?PI * N))}.

%% @spec fixp_sqrt(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc Q - square root
-spec fixp_sqrt(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
fixp_sqrt(IP, Stack, _Space) ->
	{S1, N} = pop(Stack),
	try
		R = math:sqrt(N),
		{IP, push(S1, round(R))}
	catch
		error:badarith ->
			{IP, push(S1, 0)}
	end.

%% @spec fixp_pow(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc R - pow
-spec fixp_pow(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
fixp_pow(IP, Stack, _Space) ->
	{S1, B} = pop(Stack),
	{S2, A} = pop(S1),
	try
		R = math:pow(A, B),
		{IP, push(S2, round(R))}
	catch
		error:badarith ->
			{IP, push(S2, 0)}
	end.


%% @spec fixp_signbit(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc S - signbit
-spec fixp_signbit(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
fixp_signbit(IP, Stack, _Space) ->
	{S1, N} = pop(Stack),
	{IP, push(S1, signbit(N))}.

%% @spec fixp_tan(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc T - tan
-spec fixp_tan(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
fixp_tan(IP, Stack, _Space) ->
	{S1, N} = pop(Stack),
	R = 10000 * math:tan((N / 10000) * ?F_PI_DIV_180),
	{IP, push(S1, round(R))}.

%% @spec fixp_atan(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc U - arctan
-spec fixp_atan(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
fixp_atan(IP, Stack, _Space) ->
	{S1, N} = pop(Stack),
	R = 10000 * math:atan(N / 10000) * ?F_180_DIV_PI,
	{IP, push(S1, round(R))}.

%% @spec fixp_abs(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc V - absolute value
-spec fixp_abs(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
fixp_abs(IP, Stack, _Space) ->
	{S1, N} = pop(Stack),
	{IP, push(S1, abs(N))}.

%% @spec fixp_xor(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc X - bitwise xor
-spec fixp_xor(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
fixp_xor(IP, Stack, _Space) ->
	{S1, B} = pop(Stack),
	{S2, A} = pop(S1),
	{IP, push(S2, A bxor B)}.


%% Private funtions

%% @doc This function returns the signbit for a given number.
-spec signbit(number()) -> -1 | 0 | 1.
signbit(N) when N > 0 ->  1;
signbit(N) when N < 0 -> -1;
signbit(_N)           ->  0.
