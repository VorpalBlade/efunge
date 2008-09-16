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
-module(efunge).
-export([start/2, start/1, run/1]).
-include("fip.hrl").
-include("funge_types.hrl").

%% @spec run([Filename::string()]) -> none()
%% @doc Handler for -run
-spec run([string(),...]) -> ok.
run([Filename|Parameters]) when is_list(Filename) ->
	Retval = start(Filename, Parameters),
	init:stop(Retval).

%% @spec start(string()) -> integer()
%% @doc Run efunge with a file.
-spec start(string()) -> integer().
start(Filename) when is_list(Filename) ->
	start(Filename, []).

%% @spec start(string(), list(string())) -> integer()
%% @doc Load file, set up PRNG, start main loop.
-spec start(string(), [] | [string(),...]) -> integer().
start(Filename, Parameters) when is_list(Filename) andalso is_list(Parameters) ->
	{R1,R2,R3} = now(),
	put(efungeargs, [Filename|Parameters]),
	random:seed(R1, R2, R3),
	Space = fspace:load(Filename),
	finterpreter:loop(#fip{}, fstackstack:new(), Space).
