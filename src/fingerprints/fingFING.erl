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
%% @doc FING fingerprint.
-module(fingFING).
-export([load/1]).
%% The implemented functions
-export([
         fing_swap/3,
         fing_drop/3,
         fing_push/3]).

-include("../efunge_ip.hrl").
-include("../funge_types.hrl").

%% Import common functions:
-import(efunge_stackstack, [pop/1]).
-import(efunge_fingermanager, [push_fun/3,pop_fun/2]).


%% @doc Load the FING fingerprint.
-spec load(ip()) -> {ok, ip()}.
load(IP) ->
	IP2 = efunge_fingermanager:push_funs(IP, [
		{$X, fun ?MODULE:fing_swap/3},
		{$Y, fun ?MODULE:fing_drop/3},
		{$Z, fun ?MODULE:fing_push/3}]),
	{ok, IP2}.


%% The fingerprint functions

%% @spec fing_swap(ip(), stackstack(), fungespace()) -> execute_return()
%% @doc X - Swap two semantics
-spec fing_swap(ip(), stackstack(), fungespace()) -> execute_return().
fing_swap(IP, Stack, _Space) ->
	{S1, First} = pop_op_spec(Stack),
	{S2, Second} = pop_op_spec(S1),
	if
		First =:= error; Second =:= error ->
			{efunge_ip:rev_delta(IP), S2};
		true ->
			{IP1, Fun1} = pop_fun(First, IP),
			{IP2, Fun2} = pop_fun(Second, IP1),
			IP3 = push_fun(Second, IP2, Fun1),
			IP4 = push_fun(First, IP3, Fun2),
			{IP4, S2}
	end.

%% @spec fing_drop(ip(), stackstack(), fungespace()) -> execute_return()
%% @doc Y - Drop semantic
-spec fing_drop(ip(), stackstack(), fungespace()) -> execute_return().
fing_drop(IP, Stack, _Space) ->
	case pop_op_spec(Stack) of
		{S1, error} -> {efunge_ip:rev_delta(IP), S1};
		{S1, Opcode} ->
			{IP1, _Fun} = pop_fun(Opcode, IP),
			{IP1, S1}
	end.

%% @spec fing_push(ip(), stackstack(), fungespace()) -> execute_return()
%% @doc Z - Push source semantic onto dst
-spec fing_push(ip(), stackstack(), fungespace()) -> execute_return().
fing_push(IP, Stack, _Space) ->
	{S1, Dest} = pop_op_spec(Stack),
	{S2, Src} = pop_op_spec(S1),
	if
		Dest =:= error; Src =:= error ->
			{efunge_ip:rev_delta(IP), S2};
		true ->
			{_, Fun} = pop_fun(Src, IP),
			{push_fun(Dest, IP, Fun), S2}
	end.



%% Private funtions

%% @doc Converts value to a single represenatation for the opcode stack.
-spec convert_op_spec(integer()) -> error | integer().
convert_op_spec(Value) when Value < 0   -> error;
convert_op_spec(Value) when Value =< 25 -> $A+Value;
convert_op_spec(Value) when Value < $A  -> error;
convert_op_spec(Value) when Value =< $Z -> Value;
convert_op_spec(_) -> error.

%% @doc Pop and normalise an opcode stack specification.
-spec pop_op_spec(stackstack()) -> {stackstack(), error | integer()}.
pop_op_spec(Stack) ->
	{S1, Value} = pop(Stack),
	{S1, convert_op_spec(Value)}.
