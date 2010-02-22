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
%% @doc This supervisor handles servers such as input servers and so on.
-module(efunge_supervisor_services).

-behaviour(supervisor).

%% API
-export([start_link/0, start_in_shell_for_testing/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
%% Scope for distributed Erlang: global or local
-define(SCOPE, local).

-include("otp_types.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% @spec start_link() -> {ok,Pid} | {error,Error}
%% @doc Starts the supervisor.
-spec start_link() -> otp_start_return_no_ignore().
start_link() ->
	supervisor:start_link({?SCOPE, ?SERVER}, ?MODULE, []).

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
-spec init([]) -> supervisor_return().
init([]) ->
	SupSpec     = {one_for_one,3,10},
	FungeSpace  = {'efunge_fungespace', {'efunge_fungespace', start_link, []},
	               permanent, 2000, worker, [efunge_fungespace]},
	InputServer = {'efunge_input', {'efunge_input', start_link, []},
	               permanent, 2000, worker, [efunge_input]},
	IDServer    = {'efunge_id_server', {'efunge_id_server', start_link, []},
	               permanent, 2000, worker, [efunge_id_server]},
	DataServer  = {'efunge_global_data', {'efunge_global_data', start_link, []},
	               permanent, 2000, worker, [efunge_global_data]},
	{ok,{SupSpec,[FungeSpace, InputServer, IDServer, DataServer]}}.

%%====================================================================
%% Internal functions
%%====================================================================