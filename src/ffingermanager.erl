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
%% @doc Fingerprint manager.
-module(ffingermanager).
-include("fip.hrl").
-include("funge_types.hrl").
%% These are called from core.
-export([init/1, load/2, unload/2, execute/4]).
%% These are called from fingerprint loading functions.
-export([push_fun/3]).

%% @type fingerfun() = function((ip(), stackstack(), fungespace()) -> {ip(), stackstack()}).
%%   A fingerprint function
%% @type fingerstack() = [] | list(fingerfun()).
%%   Stack is a list, access at list head.

%% Set up array of fingerprint stacks.
-spec init(ip()) -> ip().
init(#fip{} = IP) ->
	OpArray = array:new(26, [{fixed, true}, {default, []}]),
	IP#fip{ fingerOpStacks = OpArray }.

-spec load(ip(), integer()) -> ip().
load(#fip{} = IP, Fingerprint) ->
	case ffingerindex:lookup(Fingerprint) of
		notfound -> fip:rev_delta(IP);
		{_, Loader} -> Loader(IP)
	end.

-spec unload(ip(), integer()) -> ip().
unload(#fip{} = IP, Fingerprint) ->
	case ffingerindex:lookup(Fingerprint) of
		notfound -> fip:rev_delta(IP);
		{Instrs, _} -> unload_ops(IP, Instrs)
	end.

-spec execute(integer(), ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
execute(Instr, #fip{ fingerOpStacks = Array } = IP, StackStack, FungeSpace) ->
	Idx = Instr - $A,
	OpStack = array:get(Idx, Array),
	Fun = ffingerstack:peek(OpStack),
	Fun(IP, StackStack, FungeSpace).

-spec push_fun(pos_integer(), ip(), fingerfun()) -> ip().
push_fun(Instr, #fip{ fingerOpStacks = Array } = IP, Fun) when (Instr >= $A) and (Instr =< $Z) ->
	Idx = Instr - $A,
	OpStack = array:get(Idx, Array),
	S2 = ffingerstack:push(OpStack, Fun),
	Array2 = array:set(Idx, S2, Array),
	IP#fip{ fingerOpStacks = Array2 }.



%% Private functions

%% @doc Unload an op.
-spec unload_op(ip(),pos_integer()) -> ip().
unload_op(#fip{ fingerOpStacks = Array } = IP, Instr) ->
	Idx = Instr - $A,
	OpStack = array:get(Idx, Array),
	{S2, _} = ffingerstack:pop(OpStack),
	Array2 = array:set(Idx, S2, Array),
	IP#fip{ fingerOpStacks = Array2 }.

%% @doc Given a string, will unload those ops.
-spec unload_ops(ip(),string()) -> ip().
unload_ops(IP, []) ->
	IP;
unload_ops(IP, [H|T]) ->
	IP2 = unload_op(IP, H),
	unload_ops(IP2, T).
