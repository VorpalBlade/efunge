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
%% @doc Fingerprint instruction stack.
-module(ffingerstack).
-include("fip.hrl").
-include("funge_types.hrl").
-export([new/0, push/2, peek/1, pop/1]).
%% For special use:
-export([reflect/3]).

%% @type fingerfun() = function((ip(), stackstack(), fungespace()) -> {ip(), stackstack()}).
%%   A fingerprint function
%% @type fingerstack() = [] | list(fingerfun()).
%%   Stack is a list, access at list head.

%% @spec new() -> fingerstack()
%% @doc Create a new fingerprint stack.
-spec new() -> [].
new() ->
	[].

%% @spec push(fingerstack(), fingerfun()) -> fingerstack()
%% @doc Push a value on a stack.
-spec push(fingerstack(),fingerfun()) -> fingerstack().
push([], V) when is_function(V) ->
	[V];
push(L, V) when is_function(V) ->
	[V|L].

%% @spec peek(fingerstack()) -> fingerfun()
%% @doc Get the top value of a stack.
-spec peek(fingerstack()) -> fingerfun().
peek([]) ->
	fun ?MODULE:reflect/3;
peek([H|_]) when is_function(H) ->
	H.

%% @spec pop(fingerstack()) -> {fingerstack(), fingerfun()}
%% @doc Pop a value from a stack.
-spec pop(fingerstack()) -> {fingerstack(),fingerfun()}.
pop([]) ->
	{[], fun ?MODULE:reflect/3 };
pop([H|T]) when is_function(H) ->
	{T, H}.


%% @spec reflect(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc Reflect, used for reflect on empty stack.
-spec reflect(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
reflect(#fip{} = IP, Stack, _Space) ->
	{fip:rev_delta(IP), Stack}.