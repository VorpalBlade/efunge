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
%% @doc CPLI fingerprint.
-module(fingCPLI).
-include("../fip.hrl").
-include("../funge_types.hrl").
-export([load/1]).
%% The implemented functions
-export([cpli_add/3,
         cpli_div/3,
         cpli_mul/3,
         cpli_out/3,
         cpli_sub/3,
         cpli_abs/3]).

%% Import common functions:
-import(fstackstack, [push/2, pop/1]).


%% @doc Load the CPLI fingerprint.
-spec load(ip()) -> {ok, ip()}.
load(IP) ->
	IP2 = ffingermanager:push_funs(IP, [
		{$A, fun ?MODULE:cpli_add/3},
		{$D, fun ?MODULE:cpli_div/3},
		{$M, fun ?MODULE:cpli_mul/3},
		{$O, fun ?MODULE:cpli_out/3},
		{$S, fun ?MODULE:cpli_sub/3},
		{$V, fun ?MODULE:cpli_abs/3}]),
	{ok, IP2}.


%% The fingerprint functions

%% @spec cpli_add(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc A - add
-spec cpli_add(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
cpli_add(IP, Stack, _Space) ->
	{S2, Br, Bi} = pop_complex(Stack),
	{S3, Ar, Ai} = pop_complex(S2),
	S4 = push_complex(S3, Ar + Br, Ai + Bi),
	{IP, S4}.

%% @spec cpli_div(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc D - divide
-spec cpli_div(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
cpli_div(IP, Stack, _Space) ->
	{S2, Br, Bi} = pop_complex(Stack),
	{S3, Ar, Ai} = pop_complex(S2),
	Denom = Bi * Bi + Br * Br,
	if
		Denom =:= 0 ->
			S4 = push_complex(S3, 0, 0);
		true ->
			S4 = push_complex(S3, (Ai*Bi + Ar*Br) div Denom, (Ai*Br - Ar*Bi) div Denom)
	end,
	{IP, S4}.

%% @spec cpli_mul(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc M - multiply
-spec cpli_mul(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
cpli_mul(IP, Stack, _Space) ->
	{S2, Br, Bi} = pop_complex(Stack),
	{S3, Ar, Ai} = pop_complex(S2),
	S4 = push_complex(S3, Ar*Br - Ai*Bi, Ar*Bi + Ai*Br),
	{IP, S4}.

%% @spec cpli_out(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc O - output
-spec cpli_out(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
cpli_out(IP, Stack, _Space) ->
	{S2, R, I} = pop_complex(Stack),
	io:format("~w", [R]),
	if
		I > 0 -> io:put_chars([$+]);
		true  -> void
	end,
	io:format("~wi ", [I]),
	{IP, S2}.

%% @spec cpli_sub(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc S - substract
-spec cpli_sub(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
cpli_sub(IP, Stack, _Space) ->
	{S2, Br, Bi} = pop_complex(Stack),
	{S3, Ar, Ai} = pop_complex(S2),
	S4 = push_complex(S3, Ar - Br, Ai - Bi),
	{IP, S4}.

%% @spec cpli_abs(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc V - absolute value
-spec cpli_abs(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
cpli_abs(IP, Stack, _Space) ->
	{S2, R, I} = pop_complex(Stack),
	Tmp = math:sqrt(R * R + I * I),
	S3 = push(S2, round(Tmp)),
	{IP, S3}.


%% Private funtions
pop_complex(Stack) ->
	{S2, I} = pop(Stack),
	{S3, R} = pop(S2),
	{S3, R, I}.

push_complex(Stack, R, I) ->
	S2 = push(Stack, R),
	push(S2, I).
