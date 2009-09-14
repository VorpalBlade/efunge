%%%----------------------------------------------------------------------
%%% efunge - a Befunge-98 interpreter in Erlang.
%%% Copyright (C) 2008-2009 Arvid Norlander <anmaster AT tele2 DOT se>
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
%% @doc DIRF fingerprint.
-module(fingDIRF).
-export([load/1]).
%% The implemented functions
-export([
         dirf_chdir/3,
         dirf_mkdir/3,
         dirf_rmdir/3]).

-include("../efunge_ip.hrl").
-include("../funge_types.hrl").

%% Import common functions:
-import(efunge_stackstack, [pop_gnirts/1]).


%% @doc Load the DIRF fingerprint.
-spec load(ip()) -> {ok, ip()}.
load(IP) ->
	IP2 = efunge_fingermanager:push_funs(IP, [
		{$C, fun ?MODULE:dirf_chdir/3},
		{$M, fun ?MODULE:dirf_mkdir/3},
		{$R, fun ?MODULE:dirf_rmdir/3}]),
	{ok, IP2}.


%% The fingerprint functions

%% @spec dirf_chdir(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc C - change directory
-spec dirf_chdir(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
dirf_chdir(IP, Stack, _Space) ->
	{S2, Dir} = pop_gnirts(Stack),
	case file:set_cwd(Dir) of
		ok -> {IP, S2};
		{error, _Reason} -> {efunge_ip:rev_delta(IP), S2}
	end.

%% @spec dirf_mkdir(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc M - make directory
-spec dirf_mkdir(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
dirf_mkdir(IP, Stack, _Space) ->
	{S2, Dir} = pop_gnirts(Stack),
	case file:make_dir(Dir) of
		ok -> {IP, S2};
		{error, _Reason} -> {efunge_ip:rev_delta(IP), S2}
	end.

%% @spec dirf_rmdir(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}
%% @doc R - remove directory
-spec dirf_rmdir(ip(), stackstack(), fungespace()) -> {ip(), stackstack()}.
dirf_rmdir(IP, Stack, _Space) ->
	{S2, Dir} = pop_gnirts(Stack),
	case file:del_dir(Dir) of
		ok -> {IP, S2};
		{error, _Reason} -> {efunge_ip:rev_delta(IP), S2}
	end.
