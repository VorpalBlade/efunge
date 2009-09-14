%%%----------------------------------------------------------------------
%%% efunge - a Befunge-98 interpreter in Erlang.
%%% Copyright (C) 2009 Arvid Norlander <anmaster AT tele2 DOT se>
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
%% @doc ORTH fingerprint.
-module(fingORTH).
-export([load/1]).
%% The implemented functions
-export([orth_bit_and/3,
         orth_bit_xor/3,
         orth_get/3,
         orth_bit_or/3,
         orth_put/3,
         orth_output_string/3,
         orth_change_dx/3,
         orth_change_dy/3,
         orth_change_x/3,
         orth_change_y/3,
         orth_ramp_if_zero/3]).

-include("../efunge_ip.hrl").
-include("../funge_types.hrl").

%% Import common functions:
-import(efunge_stackstack, [push/2, pop/1, pop_vec/1, pop_gnirts/1]).


%% @doc Load the ORTH fingerprint.
-spec load(ip()) -> {ok, ip()}.
load(IP) ->
	IP2 = efunge_fingermanager:push_funs(IP, [
		{$A, fun ?MODULE:orth_bit_and/3},
		{$E, fun ?MODULE:orth_bit_xor/3},
		{$G, fun ?MODULE:orth_get/3},
		{$O, fun ?MODULE:orth_bit_or/3},
		{$P, fun ?MODULE:orth_put/3},
		{$S, fun ?MODULE:orth_output_string/3},
		{$V, fun ?MODULE:orth_change_dx/3},
		{$W, fun ?MODULE:orth_change_dy/3},
		{$X, fun ?MODULE:orth_change_x/3},
		{$Y, fun ?MODULE:orth_change_y/3},
		{$Z, fun ?MODULE:orth_ramp_if_zero/3}]),
	{ok, IP2}.


%% The fingerprint functions

%% @spec orth_bit_and(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc A - bitwise and
-spec orth_bit_and(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
orth_bit_and(IP, Stack, _Space) ->
	{S1, B} = pop(Stack),
	{S2, A} = pop(S1),
	{IP, push(S2, A band B)}.

%% @spec orth_bit_xor(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc E - bitwise xor
-spec orth_bit_xor(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
orth_bit_xor(IP, Stack, _Space) ->
	{S1, B} = pop(Stack),
	{S2, A} = pop(S1),
	{IP, push(S2, A bxor B)}.

%% @spec orth_get(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc G - Get cell (ortho style)
-spec orth_get(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
orth_get(IP, Stack, Space) ->
	{S1, {Y, X}} = pop_vec(Stack),
	{IP, push(S1, efunge_fungespace:fetch(Space, {X, Y}))}.

%% @spec orth_bit_or(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc O - bitwise or
-spec orth_bit_or(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
orth_bit_or(IP, Stack, _Space) ->
	{S1, B} = pop(Stack),
	{S2, A} = pop(S1),
	{IP, push(S2, A bor B)}.

%% @spec orth_put(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc P - Put cell (ortho style)
-spec orth_put(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
orth_put(IP, Stack, Space) ->
	{S1, {Y, X}} = pop_vec(Stack),
	{S2, V} = pop(S1),
	efunge_fungespace:set(Space, {X, Y}, V),
	{IP, S2}.

%% @spec orth_output_string(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc S - Output string
-spec orth_output_string(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
orth_output_string(IP, Stack, _Space) ->
	{S1, Str} = pop_gnirts(Stack),
	%% Output it
	io:format("~ts", [Str]),
	{IP, S1}.

%% @spec orth_change_dx(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc V - Change delta x
-spec orth_change_dx(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
orth_change_dx(IP, Stack, _Space) ->
	{S1, DX} = pop(Stack),
	{IP#fip{ dx = DX }, S1}.

%% @spec orth_change_dy(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc W - Change delta y
-spec orth_change_dy(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
orth_change_dy(IP, Stack, _Space) ->
	{S1, DY} = pop(Stack),
	{IP#fip{ dy = DY }, S1}.

%% @spec orth_change_x(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc X - Change x position
-spec orth_change_x(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
orth_change_x(IP, Stack, _Space) ->
	{S1, X} = pop(Stack),
	{IP#fip{ x = X }, S1}.

%% @spec orth_change_y(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc Y - Change y position
-spec orth_change_y(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
orth_change_y(IP, Stack, _Space) ->
	{S1, Y} = pop(Stack),
	{IP#fip{ y = Y }, S1}.

%% @spec orth_ramp_if_zero(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc Z - Act like trampoline if 0
-spec orth_ramp_if_zero(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
orth_ramp_if_zero(IP, Stack, Space) ->
	{S1, N} = pop(Stack),
	case N of
		0 -> {efunge_ip:ip_forward(IP, Space), S1};
		_ -> {IP, S1}
	end.
