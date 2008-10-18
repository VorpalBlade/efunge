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
%% @doc This is the top supervisor.
-module(efunge_supervisor_top).

-behaviour(supervisor).

%% API
-export([start_link/1, start_in_shell_for_testing/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
%% Scope for distributed Erlang: global or local
-define(SCOPE, local).

-include("otp_types.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the supervisor.
-spec start_link(list()) -> otp_start_return().
start_link(StartArgs) ->
	supervisor:start_link({?SCOPE, ?SERVER}, ?MODULE, StartArgs).

-spec start_in_shell_for_testing() -> pid().
start_in_shell_for_testing() ->
	{ok, Pid} = supervisor:start_link({?SCOPE, ?SERVER}, ?MODULE, []),
	unlink(Pid),
	Pid.


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}}
%% @hidden
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about restart
%% strategy, maximum restart frequency and child specifications.
-spec init(list()) -> supervisor_return().
init(StartArgs) ->
	ServiceSupervisor = {'efunge_supervisor_services',
	                     {'efunge_supervisor_services', start_link, [StartArgs]},
	                     permanent, 2000, supervisor,
	                     [efunge_supervisor_services]},
	{ok,{{one_for_one,3,10}, [ServiceSupervisor]}}.

%%====================================================================
%% Internal functions
%%====================================================================
