-module(efunge).
-export([start/2, start/1, run/1]).
-include("fip.hrl").
-include("funge_types.hrl").

%% @type state() = #fip{}.
%%    The IP and Funge state. See fstate.hrl.

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
-spec start(string(), [string(),...]) -> integer().
start(Filename, Parameters) when is_list(Filename) andalso is_list(Parameters) ->
	{R1,R2,R3} = now(),
	put(efungeargs, [Filename|Parameters]),
	random:seed(R1, R2, R3),
	Space = fspace:load(Filename),
	finterpreter:loop(#fip{}, fstackstack:new(), Space).
