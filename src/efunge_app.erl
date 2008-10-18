%%%-------------------------------------------------------------------
%%% File    : efunge_app.erl
%%% Author  : Arvid Norlander <arvid@tux.lan>
%%% Description :
%%%
%%% Created : 18 Oct 2008 by Arvid Norlander <arvid@tux.lan>
%%%-------------------------------------------------------------------
-module(efunge_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-include("otp_types.hrl").

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
-spec start(app_start_type(), any()) -> any().
start(_Type, _StartArgs) ->
	efunge_supervisor_top:start_link().

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%%--------------------------------------------------------------------
-spec stop(any()) -> ok.
stop(_State) ->
	ok.

%%====================================================================
%% Internal functions
%%====================================================================
