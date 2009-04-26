%%%----------------------------------------------------------------------
%%% efunge - a Befunge-98 interpreter in Erlang.
%%% Copyright (C) 2008-2008 Arvid Norlander <anmaster AT tele2 DOT se>
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
%% @doc Main entry point for efunge. Used to start efunge.
-module(efunge).
-export([run/1, start/1, start/2]).
-include("efunge_ip.hrl").
-include("funge_types.hrl").

%%====================================================================
%% API
%%====================================================================

%% @spec run(list(string())) -> none()
%% @doc Handler for -run
-spec run([string(),...]) -> no_return().
run([Filename|Parameters]) when is_list(Filename) ->
	%% HACK: Make unicode IO work.
	io:setopts(standard_io, [{encoding,unicode}]),
	Retval = start(Filename, Parameters),
	init:stop(Retval).

%% @spec start(string()) -> integer()
%% @doc Run efunge with a file.
-spec start(string()) -> integer().
start([_|_] = Filename) ->
	start(Filename, []).

%% @spec start(string(), list(string())) -> integer()
%% @doc Load file, set up PRNG, start main loop.
-spec start(string(), [] | [string(),...]) -> integer().
start([_|_] = Filename, Parameters) when is_list(Parameters) ->
	{R1,R2,R3} = now(),
	put(efungeargs, [Filename|Parameters]),
	random:seed(R1, R2, R3),
	Space = efunge_fungespace:create(Filename),
	IP = #fip{},
	IP2 = efunge_fingermanager:init(IP),
	{ok, _} = efunge_input:start(),
	Retval = efunge_interpreter:loop(IP2, efunge_stackstack:new(), Space),
	efunge_input:stop(),
	Retval.
