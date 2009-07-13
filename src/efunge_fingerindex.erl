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
%% @private
%% @doc This module implements a lookup "table" for fingerprints.
%% WARNING: This file is auto generated by a script. Do not change manually.
-module(efunge_fingerindex).
-include("efunge_ip.hrl").
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
%% ATHR - Asynchronous threads for Funge-98
lookup(16#41544852)  -> { "BCFGINPQRSTW", fun fingATHR:load/1 };
%% CPLI - Complex Integer extension
lookup(16#43504c49)  -> { "ADMOSV", fun fingCPLI:load/1 };
%% DIRF - Directory functions extension
lookup(16#44495246)  -> { "CMR", fun fingDIRF:load/1 };
%% FIXP - Some useful math functions
lookup(16#46495850)  -> { "ABCDIJNOPQRSTUVX", fun fingFIXP:load/1 };
%% MODU - Modulo Arithmetic
lookup(16#4d4f4455)  -> { "MRU", fun fingMODU:load/1 };
%% NULL - Null
lookup(16#4e554c4c)  -> { "ABCDEFGHIJKLMNOPQRSTUVWXYZ", fun fingNULL:load/1 };
%% ROMA - Roman Numerals
lookup(16#524f4d41)  -> { "CDILMVX", fun fingROMA:load/1 };
lookup(_Fingerprint) -> notfound.
