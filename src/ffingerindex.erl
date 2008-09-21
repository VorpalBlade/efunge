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
%% @doc This module implements a lookup "table" for fingerprints.
-module(ffingerindex).
-include("fip.hrl").
-include("funge_types.hrl").
-export([lookup/1]).

%% @type fingerloadingfun() = function((ip()) -> {ok, ip()} | {error, ip()}).
%%   A fingerprint loader function.
%% @type fingerstack() = [] | list(fingerfun()).
%%   Stack is a list, access at list head.

%% @spec lookup(integer()) -> {string(), fingerloadingfun()} | notfound
%% @doc Look up loader function and implemented instrs for a fingerprint.
%% If fingerprint isn't implemented the atom notfound will be returned.
-spec lookup(integer()) -> {string(), fingerloadingfun()} | notfound.
lookup(16#4d4f4455)  -> { "MRU",                        fun fingMODU:load/1 };
lookup(16#4e554c4c)  -> { "ABCDEFGHIJKLMNOPQRSTUVWXYZ", fun fingNULL:load/1 };
lookup(16#524f4d41)  -> { "CDILMVX",                    fun fingROMA:load/1 };
lookup(_Fingerprint) -> notfound.
