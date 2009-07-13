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
-export([run/0, start/1, start/2]).
-include("efunge_ip.hrl").
-include("funge_types.hrl").

%%====================================================================
%% API
%%====================================================================

%% @spec run(list(string())) -> none()
%% @doc Handler for -run
-spec run() -> no_return().
run()  ->
	%% HACK: Make unicode IO work.
	io:setopts(standard_io, [{encoding,unicode}]),
	[Filename|Parameters] = init:get_plain_arguments(),
	Retval = start(Filename, Parameters),
	init:stop(Retval).

%% @spec start(string()) -> integer()
%% @doc Run efunge with a file.
-spec start(string()) -> integer().
start([_|_] = Filename) ->
	start(Filename, []).

%% @spec start(string(), list(string())) -> integer()
%% @doc Load file, set up PRNG, start main loop.
-spec start(string(), [string()]) -> integer().
start([_|_] = Filename, Parameters) when is_list(Parameters) ->
	%% FIXME: This is hackish until the application bit gets properly working.
	process_flag(trap_exit, true),
	ok = application:start(efunge),
	efunge_global_data:set_cmdline([Filename|Parameters]),
	%% Load Funge-Space.
	Space = efunge_fungespace:get_fungespace(),
	ok = efunge_fungespace:load_initial(Space, Filename),
	%% Set up thread stuff:
	{ok, ThreadSupPid} = efunge_supervisor_threads:register_main(),
	{ok, _ThreadPid, _ThreadID} = efunge_supervisor_threads:create_thread(Space),
	%% FIXME: Temp hack until proper fix is done.
	receive
		{ThreadSupPid, shutdown, Retval} ->
			stop_quiet(),
			Retval;
		Other ->
			io:format("*BUG* Main got ~p. Terminating.~n", [Other]),
			stop_quiet(),
			127
	end.


%% @doc This is quite a hack: It will turn off tty logging, stop efunge then
%% turn on tty logging again.
-spec stop_quiet() -> ok.
stop_quiet() ->
	error_logger:tty(false),
	application:stop(efunge),
	error_logger:tty(true),
	ok.
