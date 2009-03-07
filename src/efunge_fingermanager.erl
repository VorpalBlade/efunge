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
-module(efunge_fingermanager).
%% These are called from core.
-export([init/1, load/2, unload/2, execute/4]).
%% These are called from fingerprint loading functions.
-export([push_fun/3, push_funs/2]).

%%====================================================================
%% Types
%%====================================================================

-include("efunge_ip.hrl").
-include("funge_types.hrl").
%% @headerfile "efunge_ip.hrl"

%% @type fingerfun() = function((ip(), stackstack(), fungespace()) -> {ip(), stackstack()}).
%%   A fingerprint function.
%% @type fingerstack() = [] | list(fingerfun()).
%%   Stack is a list, access at list head.


%%====================================================================
%% API
%%====================================================================

%% @private For use from efunge:start/2 only.
%% @doc Set up array of fingerprint stacks.
-spec init(ip()) -> ip().
init(#fip{} = IP) ->
	OpArray = array:new(26, [{fixed, true}, {default, []}]),
	IP#fip{ fingerOpStacks = OpArray }.

%% @doc Load a fingerprint.
-spec load(ip(), integer()) -> {ok | error, ip()}.
load(#fip{} = IP, Fingerprint) ->
	case efunge_fingerindex:lookup(Fingerprint) of
		notfound ->
			{error, IP};
		{_Instrs, Loader} ->
			LoaderRet = Loader(IP),
			case LoaderRet of
				{error, _} -> {error, IP};
				{ok, #fip{} = IP2} -> {ok, IP2}
			end
	end.

%% @doc Unload a fingerprint.
-spec unload(ip(), integer()) -> ip().
unload(#fip{} = IP, Fingerprint) ->
	case efunge_fingerindex:lookup(Fingerprint) of
		notfound -> efunge_ip:rev_delta(IP);
		{Instrs, _Loader} -> unload_ops(IP, Instrs)
	end.

%% @private For use from efunge_interpreter only.
%% @doc Execute a fingerprint op.
-spec execute(integer(), ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
execute(Instr, #fip{ fingerOpStacks = Array } = IP, StackStack, FungeSpace) ->
	Idx = Instr - $A,
	OpStack = array:get(Idx, Array),
	Fun = efunge_fingerstack:peek(OpStack),
	Fun(IP, StackStack, FungeSpace).

%% @doc Push a fingerprint op on the IP stack.
-spec push_fun(pos_integer(), ip(), fingerfun()) -> ip().
push_fun(Instr, #fip{ fingerOpStacks = Array } = IP, Fun) when Instr >= $A, Instr =< $Z ->
	Idx = Instr - $A,
	OpStack = array:get(Idx, Array),
	S2 = efunge_fingerstack:push(OpStack, Fun),
	Array2 = array:set(Idx, S2, Array),
	IP#fip{ fingerOpStacks = Array2 }.

%% @doc Push a list of fingerprint ops on the IP stack.
-spec push_funs(ip(),[{pos_integer(), fingerfun()}]) -> ip().
push_funs(IP, []) ->
	IP;
push_funs(IP, [{Instr,Fun}|T]) ->
	IP2 = push_fun(Instr, IP, Fun),
	push_funs(IP2, T).


%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Unload an op.
-spec unload_op(ip(),pos_integer()) -> ip().
unload_op(#fip{ fingerOpStacks = Array } = IP, Instr) ->
	Idx = Instr - $A,
	OpStack = array:get(Idx, Array),
	{S2, _} = efunge_fingerstack:pop(OpStack),
	Array2 = array:set(Idx, S2, Array),
	IP#fip{ fingerOpStacks = Array2 }.

%% @doc Given a string, will unload those ops.
-spec unload_ops(ip(),string()) -> ip().
unload_ops(IP, []) ->
	IP;
unload_ops(IP, [H|T]) ->
	IP2 = unload_op(IP, H),
	unload_ops(IP2, T).
