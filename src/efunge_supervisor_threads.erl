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
%% @doc This supervisor handle ATHR threads.
-module(efunge_supervisor_threads).

-behaviour(extended_supervisor).

%% API - OTP stuff
-export([start_link/0, start_in_shell_for_testing/0]).
%% API - Misc
-export([register_main/0]).
%% API - Thread management.
-export([create_thread/1, create_thread/3, get_threads/0]).

%% Supervisor callbacks
-export([init/1, code_change/3, handle_exit/3,handle_call/3,handle_cast/2,
         handle_info/2,terminate/2,handle_new_child/3]).

-define(SERVER, ?MODULE).
%% Scope for distributed Erlang: global or local
%-define(GLOBAL, true).

-ifdef(GLOBAL).
-define(REGISTER_NAME, {global, ?SERVER}).
-define(CALL_NAME, {global, ?SERVER}).
-else.
-define(REGISTER_NAME, {local, ?SERVER}).
-define(CALL_NAME, ?SERVER).
-endif.

-define(DICT, dict).

-include("efunge_ip.hrl").
-include("funge_types.hrl").
-include("otp_types.hrl").

-record(state,
	{
		main = none :: none | pid(),
		threads :: dict()
	}
).

-type state() :: #state{}.

%%====================================================================
%% API functions - Generic start/stop stuff
%%====================================================================

%% @spec start_link() -> {ok,Pid} | {error,Error}
%% @doc Starts the supervisor.
-spec start_link() -> otp_start_return_no_ignore().
start_link() ->
	extended_supervisor:start_link(?REGISTER_NAME, ?MODULE, []).

-spec start_in_shell_for_testing() -> pid().
start_in_shell_for_testing() ->
	{ok, Pid} = extended_supervisor:start_link(?REGISTER_NAME, ?MODULE, []),
	unlink(Pid),
	Pid.

%%====================================================================
%% API - Calls
%%====================================================================

-spec register_main() -> {ok,pid()}.
register_main() ->
	extended_supervisor:call(?CALL_NAME, reg_main).

%%====================================================================
%% API - Special calls
%%====================================================================

%% @doc Create a new thread with the supplied fungespace.
-spec create_thread(fungespace()) -> supervisor_start_child_result().
create_thread(FungeSpace) ->
	extended_supervisor:start_child(?CALL_NAME, [FungeSpace]).

%% @doc Create a new thread with the supplied fungespace, ip and stack.
-spec create_thread(fungespace(), ip(), stackstack()) -> supervisor_start_child_result().
create_thread(FungeSpace, BaseIP, BaseStackStack) ->
	extended_supervisor:start_child(?CALL_NAME, [FungeSpace, BaseIP, BaseStackStack]).

-spec get_threads() -> [pid()].
get_threads() ->
	[ Pid || {_,Pid,_,_} <- extended_supervisor:which_children(?CALL_NAME)].

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}}
%% @hidden
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about restart
%% strategy, maximum restart frequency and child specifications.
-spec init([]) -> {ok,{supervisor_spec(),[child_spec()], state()}}.
init([]) ->
	SupSpec = {simple_one_for_one,3,10},
	Thread  = {'efunge_thread', {'efunge_thread', start_link, []},
	           temporary, 2000, worker, [efunge_thread]},
	{ok,{SupSpec,[Thread],#state{threads=?DICT:new()}}}.

-spec terminate(_,state()) -> 'kill'.
terminate(_Reason, #state{} =_State) -> kill.

handle_call(reg_main, {Pid,_Tag}, #state{} = State) ->
	{reply,{ok,self()},State#state{main=Pid}};
handle_call(Request, From, #state{} = State) ->
	log_unknown(handle_call, "call", [{request, Request},{from,From}], State),
	{reply,{error,unknown_call},State}.

handle_cast(Msg, #state{} = State) ->
	log_unknown(handle_cast, "cast", [{msg, Msg}], State),
	{noreply,State}.

handle_info(Info, #state{} = State) ->
	log_unknown(handle_info, "info message", [{info, Info}], State),
	{noreply, State}.

code_change(OldVsn, #state{} = State, Extra) ->
	io:format("Unhandled code change from vsn=~p (extra=~p)!~n", [OldVsn, Extra]),
	{ok,State}.

% -spec handle_new_child(_,pid()|{pid(),integer()},state()) -> {'ok',state()}.
handle_new_child([_|_]=_Args, {Pid,ThreadID}, #state{threads=Threads} = State)
                when is_integer(ThreadID) ->
	NewThreads = ?DICT:store(Pid, ThreadID, Threads),
	{ok, State#state{threads=NewThreads}};
handle_new_child([_|_]=Args, {Pid,Extra}, #state{} = State) ->
	log_unknown(handle_new_child,"new child notification",
	            [{args,Args},{pid,Pid},{extra,Extra}], State),
	{ok, State};
handle_new_child([_|_]=Args, Pid, #state{} = State) ->
	log_unknown(handle_new_child,"new child notification",
	            [{args,Args},{pid,Pid}], State),
	{ok, State}.

% Replies:
% {Action, State}
% Action = ok | ignore | shutdown
-spec handle_exit(pid(),{shutdown,{quit|exited,integer()}}|_,state())
   -> {ok|ignore|shutdown,state()}.
%% efunge should shut down:
handle_exit(_Pid, {shutdown,{quit,Retval}}, #state{} = State) ->
	State#state.main ! {self(), shutdown, Retval},
	{shutdown, State};
%% Shut down if last thread exited.
handle_exit(Pid, {shutdown,{exited,_}}, #state{threads=Threads} = State) ->
	ThID = ?DICT:fetch(Pid, Threads),
	efunge_id_server:free_thread_id(ThID),
	NewThreads = ?DICT:erase(Pid, Threads),
	case ?DICT:size(NewThreads) of
		0 ->
			State#state.main ! {self(), shutdown, 0},
			{shutdown, State};
		_ ->
			{ok, State#state{threads=NewThreads}}
	end;
handle_exit(Pid, Reason, #state{} = State) ->
	log_unknown(handle_exit, "exit message",
	            [{pid, Pid}, {reason, Reason}], State),
	{ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

log_unknown(Function, Desc, Parameters, State) ->
	io:format("ERROR: Thread supervisor received unexpected ~s:~n", [Desc]),
	io:format("       Function:  ~p~n", [Function]),
	io:format("       Paramters: ~p~n", [Parameters]),
	io:format("       State:     ~p~n", [State]).
