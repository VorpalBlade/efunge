%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(extended_supervisor).

-behaviour(gen_server).

%% External exports
-export([start_link/2,start_link/3,
	 start_child/2, restart_child/2,
	 delete_child/2, terminate_child/2,
	 which_children/1,
	 check_childspecs/1]).

%% Extended supervisor API
-export([call/2, call/3, reply/2, multi_call/2, multi_call/3, multi_call/4,
	 cast/2, abcast/2, abcast/3]).

-export([behaviour_info/1]).

%% Internal exports
-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-export([handle_cast/2]).


-type time_triple() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type pid_undef() :: pid() | undefined.

-type sup_restart() :: one_for_one
                     | one_for_all
                     | rest_for_one
                     | simple_one_for_one.
-type sup_spec() :: {sup_restart(), non_neg_integer(), pos_integer()}.

-type supname() :: {local, atom()} | {global, atom()}.
-type supname_pid() :: supname() | {pid(),_}.
-type supref() :: atom() | {atom(), atom()} | {global, atom()} | pid().

-type child_id() :: any().
-type child_startfunc() :: {atom(),atom(),maybe_improper_list()}.
-type child_restart() :: permanent | transient | temporary.
-type child_shutdown() :: brutal_kill | pos_integer() | infinity.
-type child_type() :: worker | supervisor.
-type child_modules() :: [atom()] | dynamic.
-type child_spec() :: {child_id(),child_startfunc(),child_restart(),child_shutdown(),child_type(),child_modules()}.


-type child_spec_error_inner() :: {invalid_child_spec, _}
                                | {invalid_mfa, _}
                                | {invalid_restart_type, _}
                                | {invalid_child_type, _}
                                | {invalid_shutdown, _}
                                | {invalid_module, _}
                                | {invalid_modules, _}.
-type child_spec_error() :: {duplicate_child_name,_} | child_spec_error_inner().

-type child_start_ok_results() :: {ok, pid_undef()} | {ok, pid(), any()}.


-type which_children_result() :: {child_id() | undefined, pid_undef(), child_type(), child_modules()}.

% -type init_result() :: {ok, {sup_spec(), [child_spec()]},_} | ignore.

-type call_types() :: 'which_children'
                    | {'delete_child',_}
                    | {'restart_child',_}
                    | {'start_child',child_spec() | [any()]}
                    | {'terminate_child',child_id()}.

-type otp_start_error()  :: {error,{already_started, pid()} | any()}.
-type otp_start_return() :: {ok,pid()} | otp_start_error() | ignore.
-type otp_timeout() :: non_neg_integer() | infinity.
-type otp_timeout_or_hibernate() :: non_neg_integer() | infinity | hibernate.

-define(DICT, dict).

-record(child, {pid = undefined :: pid_undef(),  % pid is undefined when child is not running
		name :: any(),
		mfa :: child_startfunc(),
		restart_type :: child_restart(),
		shutdown :: child_shutdown(),
		child_type :: child_type(),
		modules = [] :: child_modules()}).

-record(state, {name :: supname_pid(),
		strategy :: sup_restart(),
		children = [] :: [#child{}],
		dynamics = ?DICT:new() :: dict(),
		intensity :: non_neg_integer(),
		period :: pos_integer(),
		restarts = [] :: [time_triple()],
		callbackstate :: any(),
		module :: atom(),
		args :: any()}).

-define(is_simple(State), State#state.strategy =:= simple_one_for_one).


-spec behaviour_info(callbacks | any()) -> [{atom(),non_neg_integer()},...] | undefined.
behaviour_info(callbacks) ->
    [{init,1},{handle_call,3},{handle_cast,2},{handle_info,2},
     {handle_exit,3},{terminate,2},{code_change,3}, {handle_new_child,3}];
behaviour_info(_Other) ->
    undefined.

%%% ---------------------------------------------------
%%% This is a general process supervisor built upon gen_server.erl.
%%% Servers/processes should/could also be built using gen_server.erl.
%%% SupName = {local, atom()} | {global, atom()}.
%%% ---------------------------------------------------
-spec start_link(atom(), any()) -> otp_start_return().
start_link(Mod, Args) ->
    gen_server:start_link(?MODULE, {self, Mod, Args}, []).

-spec start_link(supname(), atom(), any()) -> otp_start_return().
start_link(SupName, Mod, Args) ->
    gen_server:start_link(SupName, ?MODULE, {SupName, Mod, Args}, []).

%%% ---------------------------------------------------
%%% Interface functions.
%%% ---------------------------------------------------
-spec start_child(supref(), child_spec() | [any()]) ->
      child_start_ok_results()
      | {error, already_present | {already_started, pid_undef()} | any()}
      | {error, {start_child_callback,_}, pid()}
      | {error, {start_child_callback,_}, pid(), _}.
start_child(Supervisor, ChildSpec) ->
    call_int(Supervisor, {start_child, ChildSpec}).

-spec restart_child(supref(),child_id()) ->
      child_start_ok_results()
    | {error, running | not_found | simple_one_for_one | any()}.
restart_child(Supervisor, Name) ->
    call_int(Supervisor, {restart_child, Name}).

-spec delete_child(supref(),child_id()) -> ok | {error, running
                                                      | not_found
                                                      | simple_one_for_one}.
delete_child(Supervisor, Name) ->
    call_int(Supervisor, {delete_child, Name}).


%%% ---------------------------------------------------
%%% API functions specific to extended_supervisor.
%%% ---------------------------------------------------

-spec call(supref(),_) -> any().
call(ServerRef, Request) ->
    gen_server:call(ServerRef, {'$ext_call', Request}).

-spec call(supref(),_,otp_timeout()) -> any().
call(ServerRef, Request, Timeout) ->
    gen_server:call(ServerRef, {'$ext_call', Request}, Timeout).

-spec multi_call(atom(),_) -> any().
multi_call(Name, Request) ->
     gen_server:multi_call(Name, {'$ext_call', Request}).
-spec multi_call([atom()],atom(),_) -> any().
multi_call(Nodes, Name, Request) ->
     gen_server:multi_call(Nodes, Name, {'$ext_call', Request}).
-spec multi_call([atom()],atom(),_,otp_timeout()) -> any().
multi_call(Nodes, Name, Request, Timeout) ->
     gen_server:multi_call(Nodes, Name, {'$ext_call', Request}, Timeout).

-spec cast(supref(),_) -> 'ok'.
cast(ServerRef, Request) ->
     gen_server:cast(ServerRef, {'$ext_cast', Request}).

-spec abcast(atom(),_) -> 'abcast'.
abcast(Name, Request) ->
    gen_server:abcast(Name, {'$ext_cast', Request}).
-spec abcast([atom()],atom(),_) -> 'abcast'.
abcast(Nodes, Name, Request) ->
    gen_server:abcast(Nodes, Name, {'$ext_cast', Request}).

-spec reply({pid(), _},_) -> any().
reply(Client, Reply) ->
    gen_server:reply(Client, Reply).


%%-----------------------------------------------------------------
%% Func: terminate_child/2
%% Returns: ok | {error, Reason}
%%          Note that the child is *always* terminated in some
%%          way (maybe killed).
%%-----------------------------------------------------------------
-spec terminate_child(supref(),child_id()) -> ok | {error, not_found
                                                         | simple_one_for_one}.
terminate_child(Supervisor, Name) ->
    call_int(Supervisor, {terminate_child, Name}).

-spec which_children(supref()) -> [which_children_result()].
which_children(Supervisor) ->
    call_int(Supervisor, which_children).

-spec call_int(supref(),call_types()) -> any().
call_int(Supervisor, Req) ->
    gen_server:call(Supervisor, Req, infinity).

-spec check_childspecs([child_spec()]) -> ok | {error, _}.
check_childspecs(ChildSpecs) when is_list(ChildSpecs) ->
    case check_startspec(ChildSpecs) of
	{ok, _} -> ok;
	Error -> {error, Error}
    end;
check_childspecs(X) -> {error, {badarg, X}}.

%%% ---------------------------------------------------
%%%
%%% Initialize the supervisor.
%%%
%%% ---------------------------------------------------
-spec init({supname() | self,atom(),_}) ->
      'ignore'
    | {'ok', #state{}}
    | {'stop', 'shutdown'
             | {'bad_return',{atom(),'init',_}}
             | {'bad_start_spec',_}
             | {'start_spec',_}
             | {'supervisor_data',_}}.
init({SupName, Mod, Args}) ->
    process_flag(trap_exit, true),
    case Mod:init(Args) of
	{ok, {SupFlags, StartSpec, SupState}} ->
	    case init_state(SupName, SupFlags, Mod, Args, SupState) of
		{ok, State} when ?is_simple(State) ->
		    init_dynamic(State, StartSpec);
		{ok, State} ->
		    init_children(State, StartSpec);
		Error ->
		    {stop, {supervisor_data, Error}}
	    end;
	ignore ->
	    ignore;
	Error ->
	    {stop, {bad_return, {Mod, init, Error}}}
    end.

-spec init_children(#state{},[child_spec()]) ->
      {'ok',#state{}} | {'stop','shutdown' | {'start_spec',child_spec_error()}}.
init_children(State, StartSpec) ->
    SupName = State#state.name,
    case check_startspec(StartSpec) of
        {ok, Children} ->
            case start_children(Children, SupName) of
                {ok, NChildren} ->
                    {ok, State#state{children = NChildren}};
                {error, NChildren} ->
                    terminate_children(NChildren, SupName),
                    {stop, shutdown}
            end;
        Error ->
            {stop, {start_spec, Error}}
    end.

-spec init_dynamic(#state{}, [child_spec()]) -> {'ok',#state{}}
                                              | {'stop', {'bad_start_spec',_}
                                                       | {'start_spec',_}}.
init_dynamic(State, [StartSpec]) ->
    case check_startspec([StartSpec]) of
        {ok, Children} ->
	    {ok, State#state{children = Children}};
        Error ->
            {stop, {start_spec, Error}}
    end;
init_dynamic(_State, StartSpec) ->
    {stop, {bad_start_spec, StartSpec}}.

%%-----------------------------------------------------------------
%% Func: start_children/2
%% Args: Children = [#child] in start order
%%       SupName = {local, atom()} | {global, atom()} | {pid(),Mod}
%% Purpose: Start all children.  The new list contains #child's
%%          with pids.
%% Returns: {ok, NChildren} | {error, NChildren}
%%          NChildren = [#child] in termination order (reversed
%%                        start order)
%%-----------------------------------------------------------------
-spec start_children([#child{}], supname_pid()) -> {ok, [#child{}]}
                                                 | {error, [#child{},...]}.
start_children(Children, SupName) -> start_children(Children, [], SupName).

-spec start_children([#child{}],[#child{}],supname_pid()) -> {'error',[#child{},...]}
                                                           | {'ok',[#child{}]}.
start_children([Child|Chs], NChildren, SupName) ->
    case do_start_child(SupName, Child) of
	{ok, Pid} ->
	    start_children(Chs, [Child#child{pid = Pid}|NChildren], SupName);
	{ok, Pid, _Extra} ->
	    start_children(Chs, [Child#child{pid = Pid}|NChildren], SupName);
	{error, Reason} ->
	    report_error(start_error, Reason, Child, SupName),
	    {error, lists:reverse(Chs) ++ [Child | NChildren]}
    end;
start_children([], NChildren, _SupName) ->
    {ok, NChildren}.

-spec do_start_child(supname_pid(),#child{}) -> {'error',_}
                                              | child_start_ok_results().
do_start_child(SupName, Child) ->
    #child{mfa = {M, F, A}} = Child,
    case catch apply(M, F, A) of
	{ok, Pid} when is_pid(Pid) ->
	    NChild = Child#child{pid = Pid},
	    report_progress(NChild, SupName),
	    {ok, Pid};
	{ok, Pid, Extra} when is_pid(Pid) ->
	    NChild = Child#child{pid = Pid},
	    report_progress(NChild, SupName),
	    {ok, Pid, Extra};
	ignore ->
	    {ok, undefined};
	{error, What} -> {error, What};
	What -> {error, What}
    end.

-spec do_start_child_i(atom(),atom(),[_]) -> {'error',_}
                                           | child_start_ok_results().
do_start_child_i(M, F, A) ->
    case catch apply(M, F, A) of
	{ok, Pid} when is_pid(Pid) ->
	    {ok, Pid};
	{ok, Pid, Extra} when is_pid(Pid) ->
	    {ok, Pid, Extra};
	ignore ->
	    {ok, undefined};
	{error, Error} ->
	    {error, Error};
	What ->
	    {error, What}
    end.


%%% ---------------------------------------------------
%%%
%%% Callback functions.
%%%
%%% ---------------------------------------------------
-spec handle_call('which_children'
                | {'delete_child',child_id()}
                | {'restart_child',child_id()}
                | {'start_child', child_spec() | [any()]}
                | {'terminate_child',child_id()}
                | {'$ext_call', any()},
                  {pid(), _}, #state{}) ->
      {'reply', 'ok'
              | [which_children_result()]
              | {'error',_}
              | {'error',{start_child_callback,_},pid()}
              | {'error',{start_child_callback,_},pid(),_}
              | child_start_ok_results(),
              #state{}}
    | {reply, _, #state{}}
    | {reply, _, #state{}, otp_timeout_or_hibernate()}
    | {noreply, #state{}}
    | {noreply, #state{}, otp_timeout_or_hibernate()}
    | {stop, _, _, #state{}}
    | {stop, _, #state{}}.
handle_call({start_child, EArgs}, _From, State) when ?is_simple(State) ->
    #child{mfa = {M, F, A}} = hd(State#state.children),
    Args = A ++ EArgs,
    case do_start_child_i(M, F, Args) of
	{ok, Pid} ->
	    NState = State#state{dynamics = ?DICT:store(Pid, Args, State#state.dynamics)},
	    case (State#state.module):handle_new_child(Args, Pid, (State#state.callbackstate)) of
		{ok, CallBackState} ->
		    {reply, {ok, Pid}, NState#state{callbackstate = CallBackState}};
		Error ->
		    %% TODO: Error logging...
		    {reply, {error, {start_child_callback, Error}, Pid}, NState}
	    end;
	{ok, Pid, Extra} ->
	    NState = State#state{dynamics = ?DICT:store(Pid, Args, State#state.dynamics)},
	    case (State#state.module):handle_new_child(Args, {Pid,Extra}, (State#state.callbackstate)) of
		{ok, CallBackState} ->
		    {reply, {ok, Pid, Extra}, NState#state{callbackstate = CallBackState}};
		Error ->
		    %% TODO: Error logging...
		    {reply, {error, {start_child_callback, Error}, Pid, Extra}, NState}
	    end;
	What ->
	    {reply, What, State}
    end;

handle_call({'$ext_call',Data}, From, State) ->
    %% TODO? Catch errors?
    case (State#state.module):handle_call(Data, From, (State#state.callbackstate)) of
	{reply, Reply, CallBackState} ->
	    {reply, Reply, State#state{callbackstate = CallBackState}};
	{reply, Reply, CallBackState, Timeout} ->
	    {reply, Reply, State#state{callbackstate = CallBackState}, Timeout};
	{noreply, CallBackState} ->
	    {noreply, State#state{callbackstate = CallBackState}};
	{noreply, CallBackState, Timeout} ->
	    {noreply, State#state{callbackstate = CallBackState}, Timeout};
	{stop, Reason, Reply, CallBackState} ->
	    {stop, Reason, Reply, State#state{callbackstate = CallBackState}};
	{stop, Reason, CallBackState} ->
	    {stop, Reason, State#state{callbackstate = CallBackState}}
    end;

%%% The requests terminate_child, delete_child and restart_child are
%%% invalid for simple_one_for_one supervisors.
handle_call({_Req, _Data}, _From, State) when ?is_simple(State) ->
    {reply, {error, simple_one_for_one}, State};

handle_call({start_child, ChildSpec}, _From, State) ->
    case check_childspec(ChildSpec) of
	{ok, Child} ->
	    {Resp, NState} = handle_start_child(Child, State),
	    {reply, Resp, NState};
	What ->
	    {reply, {error, What}, State}
    end;

handle_call({restart_child, Name}, _From, State) ->
    case get_child(Name, State) of
	{value, Child} when Child#child.pid =:= undefined ->
	    case do_start_child(State#state.name, Child) of
		{ok, Pid} ->
		    NState = replace_child(Child#child{pid = Pid}, State),
		    {reply, {ok, Pid}, NState};
		{ok, Pid, Extra} ->
		    NState = replace_child(Child#child{pid = Pid}, State),
		    {reply, {ok, Pid, Extra}, NState};
		Error ->
		    {reply, Error, State}
	    end;
	{value, _} ->
	    {reply, {error, running}, State};
	_ ->
	    {reply, {error, not_found}, State}
    end;

handle_call({delete_child, Name}, _From, State) ->
    case get_child(Name, State) of
	{value, Child} when Child#child.pid =:= undefined ->
	    NState = remove_child(Child, State),
	    {reply, ok, NState};
	{value, _} ->
	    {reply, {error, running}, State};
	_ ->
	    {reply, {error, not_found}, State}
    end;

handle_call({terminate_child, Name}, _From, State) ->
    case get_child(Name, State) of
	{value, Child} ->
	    NChild = do_terminate(Child, State#state.name),
	    {reply, ok, replace_child(NChild, State)};
	_ ->
	    {reply, {error, not_found}, State}
    end;

handle_call(which_children, _From, State) when ?is_simple(State) ->
    [#child{child_type = CT, modules = Mods}] = State#state.children,
    Reply = lists:map(fun({Pid, _}) -> {undefined, Pid, CT, Mods} end,
		      ?DICT:to_list(State#state.dynamics)),
    {reply, Reply, State};

handle_call(which_children, _From, State) ->
    Resp =
	lists:map(fun(#child{pid = Pid, name = Name,
			     child_type = ChildType, modules = Mods}) ->
		    {Name, Pid, ChildType, Mods}
		  end,
		  State#state.children),
    {reply, Resp, State}.


%%%
%%% Take care of casts.
%%%
-spec handle_cast({'$ext_cast',any()},#state{})
   -> {'noreply',#state{}}
    | {'noreply',#state{}, otp_timeout_or_hibernate()}
    | {'stop',_,#state{}}.
handle_cast({'$ext_cast',Data}, State) ->
    %% TODO? Catch errors?
    case (State#state.module):handle_cast(Data, (State#state.callbackstate)) of
	{noreply, CallBackState} ->
	    {noreply, State#state{callbackstate = CallBackState}};
	{noreply, CallBackState, Timeout} ->
	    {noreply, State#state{callbackstate = CallBackState}, Timeout};
	{stop, Reason, CallBackState} ->
	    {stop, Reason, State#state{callbackstate = CallBackState}}
    end.

%%
%% Take care of terminated children and other info.
%%
%% FIXME: Should infinity as timeout really be forbidden?
-spec handle_info({'EXIT',pid(),_}|_,#state{}) ->
      {'noreply',#state{}}
    | {'noreply',#state{},'hibernate' | non_neg_integer()}
    | {'stop',_,#state{}}.
handle_info({'EXIT', Pid, Reason}, State) ->
    %% TODO? Catch errors?
    case (State#state.module):handle_exit(Pid, Reason, (State#state.callbackstate)) of
	{ok, CallBackState} ->
	    case restart_child(Pid, Reason, State#state{callbackstate = CallBackState}) of
		{ok, State1} ->
		    {noreply, State1};
		{shutdown, State1} ->
		    {stop, shutdown, State1}
	    end;
	{ignore, CallBackState} ->
	    throw(todo);
	{shutdown, CallBackState} ->
	    {stop, shutdown, State#state{callbackstate = CallBackState}}
    end;

handle_info(Msg, State) ->
    %% TODO? Catch errors?
    case (State#state.module):handle_info(Msg, (State#state.callbackstate)) of
	{noreply, CallBackState} ->
            {noreply, State#state{callbackstate = CallBackState}};
	{noreply, CallBackState, hibernate} ->
            {noreply, State#state{callbackstate = CallBackState}, hibernate};
	{noreply, CallBackState, Timeout} when is_integer(Timeout),
	                                       Timeout >= 0 ->
            {noreply, State#state{callbackstate = CallBackState}, Timeout};
	{stop, Reason, CallBackState} ->
            {stop, Reason, State#state{callbackstate = CallBackState}}
    end.

%%
%% Terminate this server.
%%
-spec terminate(normal | shutdown | {shutdown, _} | _,#state{}) -> 'ok'.
terminate(Reason, State) ->
    %% TODO? Catch errors?
    case (State#state.module):terminate(Reason, (State#state.callbackstate)) of
	nokill -> ok;
	kill ->
	    terminate_children(State#state.children, State#state.name),
	    ok
    end.

%%
%% Change code for the supervisor.
%% Call the new call-back module and fetch state and (optionally) a new start
%% specification.
%% If we got a new start specification:
%% Combine the new spec. with the old. If the new start spec. is
%% not valid the code change will not succeed.
%%
% TEST: Test code change.
-spec code_change(OldVsn::{down, _} | _,
                  State::#state{},
                  Extra::_)
   -> {ok, #state{}} | {error,_} | _.
code_change(OldVsn, State, Extra) ->
    case (State#state.module):code_change(OldVsn, State#state.callbackstate, Extra) of
	{ok, {SupFlags, StartSpec, NewState}} ->
	    case catch check_flags(SupFlags) of
		ok ->
		    {Strategy, MaxIntensity, Period} = SupFlags,
                    update_childspec(State#state{strategy = Strategy,
                                                 intensity = MaxIntensity,
                                                 period = Period,
                                                 callbackstate = NewState},
                                     StartSpec);
		Error ->
		    {error, Error}
	    end;
	{ok, NewState} ->
		NewState;
	Error ->
	    Error
    end.

-spec check_flags(sup_spec()) -> 'ok' | {bad_flags, _}.
check_flags({Strategy, MaxIntensity, Period}) ->
    validStrategy(Strategy),
    validIntensity(MaxIntensity),
    validPeriod(Period),
    ok;
check_flags(What) ->
    {bad_flags, What}.

-spec update_childspec(#state{},[child_spec()]) ->
      {error, simple_one_for_one | child_spec_error()}
    | {'ok',#state{}}.
update_childspec(State, StartSpec)  when ?is_simple(State) ->
    case check_startspec(StartSpec) of
        {ok, [Child]} ->
            {ok, State#state{children = [Child]}};
        {ok, _} ->
            {error, simple_one_for_one};
        Error ->
            {error, Error}
    end;

update_childspec(State, StartSpec) ->
    case check_startspec(StartSpec) of
	{ok, Children} ->
	    OldC = State#state.children, % In reverse start order !
	    NewC = update_childspec1(OldC, Children, []),
	    {ok, State#state{children = NewC}};
        Error ->
	    {error, Error}
    end.

-spec update_childspec1([#child{}],[#child{}],[#child{}]) -> [#child{}].
update_childspec1([Child|OldC], Children, KeepOld) ->
    case update_chsp(Child, Children) of
	{ok,NewChildren} ->
	    update_childspec1(OldC, NewChildren, KeepOld);
	false ->
	    update_childspec1(OldC, Children, [Child|KeepOld])
    end;
update_childspec1([], Children, KeepOld) ->
    % Return them in (keeped) reverse start order.
    lists:reverse(Children ++ KeepOld).

-spec update_chsp(#child{},[#child{}]) -> 'false' | {'ok',[#child{}]}.
update_chsp(OldCh, Children) ->
    case lists:map(fun(Ch) when OldCh#child.name =:= Ch#child.name ->
			   Ch#child{pid = OldCh#child.pid};
		      (Ch) ->
			   Ch
		   end,
		   Children) of
	Children ->
	    false;  % OldCh not found in new spec.
	NewC ->
	    {ok, NewC}
    end.

%%% ---------------------------------------------------
%%% Start a new child.
%%% ---------------------------------------------------
-spec handle_start_child(#child{},#state{}) ->
      {  {'error', 'already_present'}
       | {'error', {already_started, pid()}}
% FIXME: Next one may be wrong. Should pid_undef() really be there?...
       | {'error', {_, pid_undef() | #child{}}}
       | child_start_ok_results(),
       #state{}}.
handle_start_child(Child, State) ->
    case get_child(Child#child.name, State) of
	false ->
	    case do_start_child(State#state.name, Child) of
		{ok, Pid} ->
		    Children = State#state.children,
		    {{ok, Pid},
		     State#state{children =
				 [Child#child{pid = Pid}|Children]}};
		{ok, Pid, Extra} ->
		    Children = State#state.children,
		    {{ok, Pid, Extra},
		     State#state{children =
				 [Child#child{pid = Pid}|Children]}};
		{error, What} ->
		    {{error, {What, Child}}, State}
	    end;
	{value, OldChild} when OldChild#child.pid =/= undefined ->
	    {{error, {already_started, OldChild#child.pid}}, State};
	{value, _OldChild} ->
	    {{error, already_present}, State}
    end.

%%% ---------------------------------------------------
%%% Restart. A process has terminated.
%%% Returns: {ok, #state} | {shutdown, #state}
%%% ---------------------------------------------------

-spec restart_child(pid(),_,#state{}) ->
      {'ok',#state{}} | {'shutdown',#state{}}.
restart_child(Pid, Reason, State) when ?is_simple(State) ->
    case ?DICT:find(Pid, State#state.dynamics) of
	{ok, Args} ->
	    [Child] = State#state.children,
	    RestartType = Child#child.restart_type,
	    {M, F, _} = Child#child.mfa,
	    NChild = Child#child{pid = Pid, mfa = {M, F, Args}},
	    do_restart(RestartType, Reason, NChild, State);
	error ->
	    {ok, State}
    end;
restart_child(Pid, Reason, State) ->
    Children = State#state.children,
    case lists:keysearch(Pid, #child.pid, Children) of
	{value, Child} ->
	    RestartType = Child#child.restart_type,
	    do_restart(RestartType, Reason, Child, State);
	_ ->
	    {ok, State}
    end.

-spec do_restart(child_restart(),_,#child{},#state{}) ->
      {'ok',#state{}} | {'shutdown',#state{}}.
do_restart(permanent, Reason, Child, State) ->
    report_error(child_terminated, Reason, Child, State#state.name),
    restart(Child, State);
do_restart(_, normal, Child, State) ->
    NState = state_del_child(Child, State),
    {ok, NState};
do_restart(_, shutdown, Child, State) ->
    NState = state_del_child(Child, State),
    {ok, NState};
do_restart(transient, Reason, Child, State) ->
    report_error(child_terminated, Reason, Child, State#state.name),
    restart(Child, State);
do_restart(temporary, Reason, Child, State) ->
    report_error(child_terminated, Reason, Child, State#state.name),
    NState = state_del_child(Child, State),
    {ok, NState}.

-spec restart(#child{},#state{}) ->
      {'ok',#state{}} | {'shutdown',#state{}}.
restart(Child, State) ->
    case add_restart(State) of
	{ok, NState} ->
	    restart(NState#state.strategy, Child, NState);
	{terminate, NState} ->
	    report_error(shutdown, reached_max_restart_intensity,
			 Child, State#state.name),
	    {shutdown, remove_child(Child, NState)}
    end.

-spec restart(sup_restart(),#child{},#state{}) ->
      {'ok',#state{}} | {'shutdown',#state{}}.
restart(simple_one_for_one, Child, State) ->
    #child{mfa = {M, F, A}} = Child,
    Dynamics = ?DICT:erase(Child#child.pid, State#state.dynamics),
    case do_start_child_i(M, F, A) of
	{ok, Pid} ->
	    NState = State#state{dynamics = ?DICT:store(Pid, A, Dynamics)},
	    {ok, NState};
	{ok, Pid, _Extra} ->
	    NState = State#state{dynamics = ?DICT:store(Pid, A, Dynamics)},
	    {ok, NState};
	{error, Error} ->
	    report_error(start_error, Error, Child, State#state.name),
	    restart(Child, State)
    end;
restart(one_for_one, Child, State) ->
    case do_start_child(State#state.name, Child) of
	{ok, Pid} ->
	    NState = replace_child(Child#child{pid = Pid}, State),
	    {ok, NState};
	{ok, Pid, _Extra} ->
	    NState = replace_child(Child#child{pid = Pid}, State),
	    {ok, NState};
	{error, Reason} ->
	    report_error(start_error, Reason, Child, State#state.name),
	    restart(Child, State)
    end;
restart(rest_for_one, Child, State) ->
    {ChAfter, ChBefore} = split_child(Child#child.pid, State#state.children),
    ChAfter2 = terminate_children(ChAfter, State#state.name),
    case start_children(ChAfter2, State#state.name) of
	{ok, ChAfter3} ->
	    {ok, State#state{children = ChAfter3 ++ ChBefore}};
	{error, ChAfter3} ->
	    restart(Child, State#state{children = ChAfter3 ++ ChBefore})
    end;
restart(one_for_all, Child, State) ->
    Children1 = del_child(Child#child.pid, State#state.children),
    Children2 = terminate_children(Children1, State#state.name),
    case start_children(Children2, State#state.name) of
	{ok, NChs} ->
	    {ok, State#state{children = NChs}};
	{error, NChs} ->
	    restart(Child, State#state{children = NChs})
    end.

%%-----------------------------------------------------------------
%% Func: terminate_children/2
%% Args: Children = [#child] in termination order
%%       SupName = {local, atom()} | {global, atom()} | {pid(),Mod}
%% Returns: NChildren = [#child] in
%%          startup order (reversed termination order)
%%-----------------------------------------------------------------
-spec terminate_children([#child{}],supname_pid()) -> [#child{pid::'undefined'}].
terminate_children(Children, SupName) ->
    terminate_children(Children, SupName, []).

-spec terminate_children([#child{}],supname_pid(),[#child{pid::'undefined'}]) ->
      [#child{pid::'undefined'}].
terminate_children([Child | Children], SupName, Res) ->
    NChild = do_terminate(Child, SupName),
    terminate_children(Children, SupName, [NChild | Res]);
terminate_children([], _SupName, Res) ->
    Res.

-spec do_terminate(#child{},supname_pid()) -> #child{pid::'undefined'}.
do_terminate(Child, SupName) when Child#child.pid =/= undefined ->
    case shutdown(Child#child.pid,
		  Child#child.shutdown) of
	ok ->
	    Child#child{pid = undefined};
	{error, OtherReason} ->
	    report_error(shutdown_error, OtherReason, Child, SupName),
	    Child#child{pid = undefined}
    end;
do_terminate(Child, _SupName) ->
    Child.

%%-----------------------------------------------------------------
%% Shutdowns a child. We must check the EXIT value
%% of the child, because it might have died with another reason than
%% the wanted. In that case we want to report the error. We put a
%% monitor on the child an check for the 'DOWN' message instead of
%% checking for the 'EXIT' message, because if we check the 'EXIT'
%% message a "naughty" child, who does unlink(Sup), could hang the
%% supervisor.
%% Returns: ok | {error, OtherReason}  (this should be reported)
%%-----------------------------------------------------------------
-spec shutdown(pid(),child_shutdown()) -> 'ok' | {'error',_}.
shutdown(Pid, brutal_kill) ->

    case monitor_child(Pid) of
	ok ->
	    exit(Pid, kill),
	    receive
		{'DOWN', _MRef, process, Pid, killed} ->
		    ok;
		{'DOWN', _MRef, process, Pid, OtherReason} ->
		    {error, OtherReason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end;

shutdown(Pid, Time) ->

    case monitor_child(Pid) of
	ok ->
	    exit(Pid, shutdown), %% Try to shutdown gracefully
	    receive
		{'DOWN', _MRef, process, Pid, shutdown} ->
		    ok;
		{'DOWN', _MRef, process, Pid, OtherReason} ->
		    {error, OtherReason}
	    after Time ->
		    exit(Pid, kill),  %% Force termination.
		    receive
			{'DOWN', _MRef, process, Pid, OtherReason} ->
			    {error, OtherReason}
		    end
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%% Help function to shutdown/2 switches from link to monitor approach
-spec monitor_child(pid()) -> 'ok' | {'error',_}.
monitor_child(Pid) ->

    %% Do the monitor operation first so that if the child dies
    %% before the monitoring is done causing a 'DOWN'-message with
    %% reason noproc, we will get the real reason in the 'EXIT'-message
    %% unless a naughty child has already done unlink...
    erlang:monitor(process, Pid),
    unlink(Pid),

    receive
	%% If the child dies before the unlik we must empty
	%% the mail-box of the 'EXIT'-message and the 'DOWN'-message.
	{'EXIT', Pid, Reason} ->
	    receive
		{'DOWN', _, process, Pid, _} ->
		    {error, Reason}
	    end
    after 0 ->
	    %% If a naughty child did unlink and the child dies before
	    %% monitor the result will be that shutdown/2 receives a
	    %% 'DOWN'-message with reason noproc.
	    %% If the child should die after the unlink there
	    %% will be a 'DOWN'-message with a correct reason
	    %% that will be handled in shutdown/2.
	    ok
    end.


%%-----------------------------------------------------------------
%% Child/State manipulating functions.
%%-----------------------------------------------------------------
-spec state_del_child(#child{},#state{}) -> #state{}.
state_del_child(#child{pid = Pid}, State) when ?is_simple(State) ->
    NDynamics = ?DICT:erase(Pid, State#state.dynamics),
    State#state{dynamics = NDynamics};
state_del_child(Child, State) ->
    NChildren = del_child(Child#child.name, State#state.children),
    State#state{children = NChildren}.

-spec del_child(pid()|_,[#child{}]) -> [#child{}].
del_child(Name, [Ch|Chs]) when Ch#child.name =:= Name ->
    [Ch#child{pid = undefined} | Chs];
del_child(Pid, [Ch|Chs]) when Ch#child.pid =:= Pid ->
    [Ch#child{pid = undefined} | Chs];
del_child(Name, [Ch|Chs]) ->
    [Ch|del_child(Name, Chs)];
del_child(_, []) ->
    [].

%% Chs = [S4, S3, Ch, S1, S0]
%% Ret: {[S4, S3, Ch], [S1, S0]}
-spec split_child(pid_undef(),[#child{}]) -> {[#child{}],[#child{}]}.
split_child(Name, Chs) ->
    split_child(Name, Chs, []).

-spec split_child(pid_undef(),[#child{}],[#child{}]) -> {[#child{}],[#child{}]}.
split_child(Name, [Ch|Chs], After) when Ch#child.name =:= Name ->
    {lists:reverse([Ch#child{pid = undefined} | After]), Chs};
split_child(Pid, [Ch|Chs], After) when Ch#child.pid =:= Pid ->
    {lists:reverse([Ch#child{pid = undefined} | After]), Chs};
split_child(Name, [Ch|Chs], After) ->
    split_child(Name, Chs, [Ch | After]);
split_child(_, [], After) ->
    {lists:reverse(After), []}.

-spec get_child(_,#state{}) -> 'false' | {'value',#child{}}.
get_child(Name, State) ->
    lists:keysearch(Name, #child.name, State#state.children).

-spec replace_child(#child{},#state{children::[#child{},...]}) ->
      #state{children::[#child{},...]}.
replace_child(Child, State) ->
    Chs = do_replace_child(Child, State#state.children),
    State#state{children = Chs}.

-spec do_replace_child(#child{},[#child{},...]) -> [#child{},...].
do_replace_child(Child, [Ch|Chs]) when Ch#child.name =:= Child#child.name ->
    [Child | Chs];
do_replace_child(Child, [Ch|Chs]) ->
    [Ch|do_replace_child(Child, Chs)].

-spec remove_child(#child{},#state{}) -> #state{}.
remove_child(Child, State) ->
    Chs = lists:keydelete(Child#child.name, #child.name, State#state.children),
    State#state{children = Chs}.

%%-----------------------------------------------------------------
%% Func: init_state/5
%% Args: SupName = {local, atom()} | {global, atom()} | self
%%       Type = {Strategy, MaxIntensity, Period}
%%         Strategy = one_for_one | one_for_all | simple_one_for_one |
%%                    rest_for_one
%%         MaxIntensity = integer()
%%         Period = integer()
%%       Mod :== atom()
%%       Arsg :== term()
%%       SupState :== any()
%% Purpose: Check that Type is of correct type (!)
%% Returns: {ok, #state} | Error
%%-----------------------------------------------------------------
-spec init_state(self | supname(),sup_spec(),atom(),_,_) ->
      {'invalid_type',_}
    | {'ok',#state{children::[], restarts::[]}}.
init_state(SupName, Type, Mod, Args, SupState) ->
    case catch init_state1(SupName, Type, Mod, Args) of
	{ok, State} ->
	    {ok, State#state{callbackstate = SupState}};
	Error ->
	    Error
    end.

-spec init_state1(self | supname(),sup_spec(),atom(),_) ->
      {'invalid_type',_}
    | {'ok',#state{children::[], restarts::[]}}.
init_state1(SupName, {Strategy, MaxIntensity, Period}, Mod, Args) ->
    validStrategy(Strategy),
    validIntensity(MaxIntensity),
    validPeriod(Period),
    {ok, #state{name = supname(SupName,Mod),
	       strategy = Strategy,
	       intensity = MaxIntensity,
	       period = Period,
	       module = Mod,
	       args = Args}};
init_state1(_SupName, Type, _, _) ->
    {invalid_type, Type}.

-spec validStrategy(sup_restart()) -> 'true'.
validStrategy(simple_one_for_one) -> true;
validStrategy(one_for_one)        -> true;
validStrategy(one_for_all)        -> true;
validStrategy(rest_for_one)       -> true;
validStrategy(What)               -> throw({invalid_strategy, What}).

-spec validIntensity(non_neg_integer()) -> 'true'.
validIntensity(Max) when is_integer(Max),
                         Max >=  0 -> true;
validIntensity(What)              -> throw({invalid_intensity, What}).

-spec validPeriod(pos_integer()) -> 'true'.
validPeriod(Period) when is_integer(Period),
                         Period > 0 -> true;
validPeriod(What)                   -> throw({invalid_period, What}).

-spec supname(self | supname(),atom()) -> supname_pid().
supname(self,Mod) -> {self(),Mod};
supname(N,_)      -> N.

%%% ------------------------------------------------------
%%% Check that the children start specification is valid.
%%% Shall be a six (6) tuple
%%%    {Name, Func, RestartType, Shutdown, ChildType, Modules}
%%% where Name is an atom
%%%       Func is {Mod, Fun, Args} == {atom, atom, list}
%%%       RestartType is permanent | temporary | transient
%%%       Shutdown = integer() | infinity | brutal_kill
%%%       ChildType = supervisor | worker
%%%       Modules = [atom()] | dynamic
%%% Returns: {ok, [#child]} | Error
%%% ------------------------------------------------------

-spec check_startspec([child_spec()]) ->
      {'ok',[#child{}]} | child_spec_error().
check_startspec(Children) -> check_startspec(Children, []).

-spec check_startspec([child_spec()],[#child{}]) ->
      {'ok',[#child{}]} | child_spec_error().
check_startspec([ChildSpec|T], Res) ->
    case check_childspec(ChildSpec) of
	{ok, Child} ->
	    case lists:keysearch(Child#child.name, #child.name, Res) of
		{value, _} -> {duplicate_child_name, Child#child.name};
		_ -> check_startspec(T, [Child | Res])
	    end;
	Error -> Error
    end;
check_startspec([], Res) ->
    {ok, lists:reverse(Res)}.

-spec check_childspec(child_spec()) ->
      {'ok',#child{pid::'undefined'}} | child_spec_error_inner().
check_childspec({Name, Func, RestartType, Shutdown, ChildType, Mods}) ->
    catch check_childspec(Name, Func, RestartType, Shutdown, ChildType, Mods);
check_childspec(X) -> {invalid_child_spec, X}.

-spec check_childspec(_,child_startfunc(),child_restart(),child_shutdown(),child_type(),child_modules()) ->
      {'ok',#child{pid::'undefined'}}.
check_childspec(Name, Func, RestartType, Shutdown, ChildType, Mods) ->
    validName(Name),
    validFunc(Func),
    validRestartType(RestartType),
    validChildType(ChildType),
    validShutdown(Shutdown, ChildType),
    validMods(Mods),
    {ok, #child{name = Name, mfa = Func, restart_type = RestartType,
		shutdown = Shutdown, child_type = ChildType, modules = Mods}}.

-spec validChildType(child_type()) -> true.
validChildType(supervisor)  -> true;
validChildType(worker) -> true;
validChildType(What)  -> throw({invalid_child_type, What}).

-spec validName(_) -> 'true'.
validName(_Name) -> true.

-spec validFunc(child_startfunc()) -> 'true'.
validFunc({M, F, A}) when is_atom(M),
                          is_atom(F),
                          is_list(A) -> true;
validFunc(Func)                      -> throw({invalid_mfa, Func}).

-spec validRestartType(child_restart()) -> 'true'.
validRestartType(permanent)   -> true;
validRestartType(temporary)   -> true;
validRestartType(transient)   -> true;
validRestartType(RestartType) -> throw({invalid_restart_type, RestartType}).

-spec validShutdown(child_shutdown(),child_type()) -> 'true'.
validShutdown(Shutdown, _)
  when is_integer(Shutdown), Shutdown > 0 -> true;
validShutdown(infinity, supervisor)    -> true;
validShutdown(brutal_kill, _)          -> true;
validShutdown(Shutdown, _)             -> throw({invalid_shutdown, Shutdown}).

-spec validMods(child_modules()) -> 'ok' | 'true'.
validMods(dynamic) -> true;
validMods(Mods) when is_list(Mods) ->
    lists:foreach(fun(Mod) ->
		    if
			is_atom(Mod) -> ok;
			true -> throw({invalid_module, Mod})
		    end
		  end,
		  Mods);
validMods(Mods) -> throw({invalid_modules, Mods}).

%%% ------------------------------------------------------
%%% Add a new restart and calculate if the max restart
%%% intensity has been reached (in that case the supervisor
%%% shall terminate).
%%% All restarts accured inside the period amount of seconds
%%% are kept in the #state.restarts list.
%%% Returns: {ok, State'} | {terminate, State'}
%%% ------------------------------------------------------

-spec add_restart(#state{}) -> {'ok',#state{}}
                             | {'terminate',#state{}}.
add_restart(State) ->
    I = State#state.intensity,
    P = State#state.period,
    R = State#state.restarts,
    Now = erlang:now(),
    R1 = add_restart([Now|R], Now, P),
    State1 = State#state{restarts = R1},
    case length(R1) of
	CurI when CurI  =< I ->
	    {ok, State1};
	_ ->
	    {terminate, State1}
    end.

-spec add_restart([time_triple()], time_triple(), pos_integer()) -> [time_triple()].
add_restart([R|Restarts], Now, Period) ->
    case inPeriod(R, Now, Period) of
	true ->
	    [R|add_restart(Restarts, Now, Period)];
	_ ->
	    []
    end;
add_restart([], _, _) ->
    [].

-spec inPeriod(time_triple(), time_triple(), pos_integer()) -> bool().
inPeriod(Time, Now, Period) ->
    case difference(Time, Now) of
	T when T > Period ->
	    false;
	_ ->
	    true
    end.

%%
%% Time = {MegaSecs, Secs, MicroSecs} (NOTE: MicroSecs is ignored)
%% Calculate the time elapsed in seconds between two timestamps.
%% If MegaSecs is equal just subtract Secs.
%% Else calculate the Mega difference and add the Secs difference,
%% note that Secs difference can be negative, e.g.
%%      {827, 999999, 676} diff {828, 1, 653753} == > 2 secs.
%%
-spec difference(time_triple(), time_triple()) -> non_neg_integer().
difference({TimeM, TimeS, _}, {CurM, CurS, _}) when CurM > TimeM ->
    ((CurM - TimeM) * 1000000) + (CurS - TimeS);
difference({_, TimeS, _}, {_, CurS, _}) ->
    CurS - TimeS.

%%% ------------------------------------------------------
%%% Error and progress reporting.
%%% ------------------------------------------------------

-spec report_error('child_terminated' | 'shutdown' | 'shutdown_error' | 'start_error',
                   'reached_max_restart_intensity' | any(), #child{}, supname_pid()) -> 'ok'.
report_error(Error, Reason, Child, SupName) ->
    ErrorMsg = [{supervisor, SupName},
		{errorContext, Error},
		{reason, Reason},
		{offender, extract_child(Child)}],
    error_logger:error_report(supervisor_report, ErrorMsg).


-spec extract_child(#child{}) ->
      [{'child_type', child_type()}
     | {'mfa', child_startfunc()}
     | {'name', _}
     | {'pid', pid_undef()}
     | {'restart_type', child_restart()}
     | {'shutdown', child_shutdown()},...].
extract_child(Child) ->
    [{pid, Child#child.pid},
     {name, Child#child.name},
     {mfa, Child#child.mfa},
     {restart_type, Child#child.restart_type},
     {shutdown, Child#child.shutdown},
     {child_type, Child#child.child_type}].

-spec report_progress(#child{},supname_pid()) -> 'ok'.
report_progress(Child, SupName) ->
    Progress = [{supervisor, SupName},
		{started, extract_child(Child)}],
    error_logger:info_report(progress, Progress).
