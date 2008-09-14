-module(efunge).
-export([start/1, run/1]).
-include("fip.hrl").
-include("funge_types.hrl").

%% @type state() = #fip{}.
%%    The IP and Funge state. See fstate.hrl.

%% @spec run([Filename::string()]) -> none()
%% @doc Handler for -run
-spec run([string(),...]) -> ok.
run([Filename]) when is_list(Filename) ->
	Retval = start(Filename),
	init:stop(Retval).

%% @spec start(string()) -> integer()
%% @doc Load file, set up PRNG, start main loop.
-spec start(string()) -> integer().
start(Filename) when is_list(Filename) ->
	{R1,R2,R3} = now(),
	random:seed(R1, R2, R3),
	Space = fspace:load(Filename),
	finterpreter:loop(#fip{}, fstackstack:new(), Space).
