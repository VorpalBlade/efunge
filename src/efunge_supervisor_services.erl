%%%----------------------------------------------------------------------
%%% efunge - a Befunge-98 interpreter in Erlang.
%%% Copyright (C) 2008-2010 Arvid Norlander <anmaster AT tele2 DOT se>
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
-export([add_service/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
%% Define for global server (in distributed Erlang).
%-define(GLOBAL, true).

-ifdef(GLOBAL).
-define(REGISTER_NAME, {global, ?SERVER}).
-define(CALL_NAME, {global, ?SERVER}).
-else.
-define(REGISTER_NAME, {local, ?SERVER}).
-define(CALL_NAME, ?SERVER).
-endif.


-include("otp_types.hrl").

%%====================================================================
%% API functions
%%====================================================================

%% @spec start_link() -> {ok,Pid} | {error,Error}
%% @doc Starts the supervisor.
-spec start_link() -> otp_start_return_no_ignore().
start_link() ->
	supervisor:start_link(?REGISTER_NAME, ?MODULE, []).

-spec start_in_shell_for_testing() -> pid().
start_in_shell_for_testing() ->
	{ok, Pid} = supervisor:start_link(?REGISTER_NAME, ?MODULE, []),
	unlink(Pid),
	Pid.


%% @doc
%% Adds a new service (intended for start on demand services).
%% Be careful when using!
%%
%% This function is just a wrapper providing the first parameter and
%% handling {error,already_present).
%% The caller is responsible for handling any other errors.
%% For already_present: will return {already_present,RestartResult}
%%
%% For details see documentation for supervisor:start_child.
-spec add_service(supervisor:child_spec()) -> supervisor_child_ok()
                                            | {error, supervisor_child_error_basic()}
                                            | {already_present, supervisor_child_ok()
                                                              | {error, supervisor_child_error()}}.
add_service(ChildSpec = {Id,_StartFunc,_Restart,_Shutdown,_Type,_Modules}) ->
	case supervisor:start_child(?CALL_NAME, ChildSpec) of
		{error,already_present} ->
			{already_present, supervisor:restart_child(?CALL_NAME, Id)};
		Result -> Result
	end.

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
