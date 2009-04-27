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
%% @doc Thread ID allocator/manager.
-module(efunge_id_server).

-behaviour(gen_server).

%% API - OTP stuff
-export([start/0, start_link/0, stop/0]).
%% API - Calls to server.
-export([alloc_thread_id/0, alloc_ip_id/1,
         free_thread_id/1, free_ip_id/1,
         lookup_thread/1, lookup_ip_pid/1, lookup_ip_thread/1,
         get_all_threads/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

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

%%====================================================================
%% Types
%%====================================================================

-record(state,
	{
		next_free_thread = 0 :: integer(),
		next_free_ip = 0 :: integer()
	}
).

-type state()     :: #state{}.
-type thread_id() :: integer().
-type ip_id()     :: integer().

-type call_return_replies() :: notfound | ok | pid() | thread_id() | ip_id() | [pid()].
-type call_return_reply()   :: {reply, call_return_replies(), state()}.
-type call_return_stop()    :: {stop,normal,stopped,state()}.
-type call_return()         :: call_return_reply() | call_return_stop().
-type call_op_none()        :: get_all_threads | alloc_thread_id | stop.
-type call_op_ipid()        :: free_ip_id | lookup_ip_pid | lookup_ip_thread.
-type call_op_threadid()    :: alloc_ip_id | free_thread_id | lookup_thread.
-type call_op()             :: call_op_none()
                            | {call_op_ipid(), ip_id()}
                            | {call_op_threadid(), thread_id()}.

-include("otp_types.hrl").


%%====================================================================
%% API - Generic start/stop stuff
%%====================================================================

%% @spec start_link() -> {ok,Pid} | {error,Error}
%% @doc Starts the server, linked to supervisor.
-spec start_link() -> otp_start_return_no_ignore().
start_link() ->
	gen_server:start_link(?REGISTER_NAME, ?MODULE, [], []).

%% @spec start() -> {ok,Pid} | {error,Error}
%% @doc Starts the server, standalone.
-spec start() -> otp_start_return_no_ignore().
start() ->
	gen_server:start(?REGISTER_NAME, ?MODULE, [], []).

%% @spec stop() -> stopped
%% @doc Stops the server. Use only for standalone server.
-spec stop() -> stopped.
stop() ->
	gen_server:call(?CALL_NAME, stop).


%%====================================================================
%% API - Calls
%%====================================================================

%% @doc Allocate a thread ID.
-spec alloc_thread_id() -> thread_id().
alloc_thread_id() ->
	gen_server:call(?CALL_NAME, alloc_thread_id).

%% @doc Allocate an IP ID for a thread.
-spec alloc_ip_id(thread_id()) -> ip_id() | notfound.
alloc_ip_id(ThreadID) when is_integer(ThreadID) ->
	gen_server:call(?CALL_NAME, {alloc_ip_id, ThreadID}).

%% @doc Free a thread ID (also freeing all related threads).
-spec free_thread_id(thread_id()) -> ok | notfound.
free_thread_id(ThreadID) when is_integer(ThreadID) ->
	gen_server:call(?CALL_NAME, {free_thread_id, ThreadID}).

%% @doc Free an IP ID.
-spec free_ip_id(ip_id()) -> ok | notfound.
free_ip_id(IpID) when is_integer(IpID) ->
	gen_server:call(?CALL_NAME, {free_ip_id, IpID}).

%% @doc Get PID for a thread.
-spec lookup_thread(thread_id()) -> notfound | pid().
lookup_thread(ThreadID) when is_integer(ThreadID)  ->
	gen_server:call(?CALL_NAME, {lookup_thread, ThreadID}).

%% @doc Get thread ID for an IP.
-spec lookup_ip_thread(ip_id()) -> notfound | thread_id().
lookup_ip_thread(IpID) when is_integer(IpID) ->
	gen_server:call(?CALL_NAME, {lookup_ip_thread, IpID}).

%% @doc Get thread PID for an IP.
-spec lookup_ip_pid(ip_id()) -> notfound | pid().
lookup_ip_pid(IpID) when is_integer(IpID) ->
	gen_server:call(?CALL_NAME, {lookup_ip_pid, IpID}).

%% @doc Get thread PIDs.
-spec get_all_threads() -> [pid()].
get_all_threads() ->
	gen_server:call(?CALL_NAME, get_all_threads).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%% @hidden
%% @doc Initiates the server
-spec init([]) -> {ok, #state{}}.
init([]) ->
	process_flag(trap_exit, true),
	create_ets_tables(),
	{ok, #state{}}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} | {stop, Reason, Reply, State}
%% @hidden
%% @doc Handling call messages
-spec handle_call(call_op(), {pid(), _} ,state()) -> call_return().
handle_call(alloc_thread_id, {Pid, _Tag} = _From, State) ->
	{NewState, ThreadID} = get_next_thread_id(State),
	add_thread(ThreadID, Pid),
	{reply, ThreadID, NewState};

handle_call({alloc_ip_id, ThreadID}, _From, State) ->
	{NewState, IpID} = get_next_ip_id(State),
	add_ip(ThreadID, IpID),
	{reply, IpID, NewState};

handle_call({free_thread_id, ThreadID}, _From, State) ->
	free_all_ips(ThreadID),
	Reply = remove_thread(ThreadID),
	{reply, Reply, State};

handle_call({free_ip_id, IpID}, _From, State) ->
	Reply = remove_ip(IpID),
	{reply, Reply, State};

handle_call({lookup_thread, ThreadID}, _From, State) ->
	{reply, thread_to_pid(ThreadID), State};

handle_call({lookup_ip_thread, IpID}, _From, State) ->
	{reply, ip_to_thread(IpID), State};

handle_call({lookup_ip_pid, IpID}, _From, State) ->
	{reply, thread_to_pid(ip_to_thread(IpID)), State};

handle_call(get_all_threads, _From, State) ->
	{reply, get_all_thread_pids(), State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State}.

%% @spec handle_cast(Msg, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
%% @hidden
%% @doc Handling cast messages
-spec handle_cast(_,state()) -> {noreply,state()}.
handle_cast(_Msg, State) ->
	{noreply, State}.

%% @spec handle_info(Info, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
%% @hidden
%% @doc Handling all non call/cast messages
-spec handle_info(_,state()) -> {noreply,state()}.
handle_info(_Info, State) ->
	{noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @hidden
%% @doc Clean up on quit
-spec terminate(_,state()) -> ok.
terminate(_Reason, _State) ->
	destroy_ets_tables().

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @hidden
%% @doc Convert process state when code is changed
-spec code_change(_,state(),_) -> {'ok',state()}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

-spec create_ets_tables() -> ok.
create_ets_tables() ->
	ets:new(thread_to_pid, [set, private, named_table]),
	ets:new(ip_to_thread,  [set, private, named_table]),
	ets:new(thread_to_ips, [bag, private, named_table]),
	ok.

-spec destroy_ets_tables() -> ok.
destroy_ets_tables() ->
	ets:delete(thread_to_pid),
	ets:delete(ip_to_thread),
	ets:delete(thread_to_ips),
	ok.

%% Add a thread
-spec add_thread(thread_id(),pid()) -> true.
add_thread(ThreadID, Pid) ->
	ets:insert(thread_to_pid, {ThreadID, Pid}).

%% Remove a thread
-spec remove_thread(thread_id()) -> ok.
remove_thread(ThreadID) ->
	ets:delete(thread_to_ips, ThreadID),
	ets:delete(thread_to_pid, ThreadID),
	ok.

%% Look up pid for thread ID.
-spec thread_to_pid(notfound | thread_id()) -> notfound | pid().
thread_to_pid(notfound) ->
	notfound;
thread_to_pid(ThreadID) ->
	case ets:lookup(thread_to_pid, ThreadID) of
		[]                -> notfound;
		[{ThreadID, Pid}] -> Pid
	end.

-spec get_all_thread_pids() -> [pid()].
get_all_thread_pids() ->
	ets:select(thread_to_pid,
	           [{{'_','$2'},[],['$2']}]).

%% Add an IP
-spec add_ip(thread_id(), ip_id()) -> true.
add_ip(ThreadID, IpID) ->
	ets:insert(ip_to_thread, {IpID, ThreadID}),
	ets:insert(thread_to_ips, {ThreadID, IpID}).

%% Remove an IP
-spec remove_ip(ip_id()) -> notfound | ok.
remove_ip(IpID) ->
	case ip_to_thread(IpID) of
		notfound -> notfound;
		ThreadID ->
			ets:delete(ip_to_thread, IpID),
			ets:delete_object(thread_to_ips, {ThreadID, IpID}),
			ok
	end.

%% Look up Thread for IP
-spec ip_to_thread(ip_id()) -> notfound | thread_id().
ip_to_thread(IpID) ->
	case ets:lookup(ip_to_thread, IpID) of
		[]                 -> notfound;
		[{IpID, ThreadID}] -> ThreadID
	end.

%% Returns a free Thread ID
-spec get_next_thread_id(state()) -> {state(),thread_id()}.
get_next_thread_id(#state{next_free_thread=NextID} = State ) ->
	{State#state{next_free_thread=NextID+1}, NextID}.

%% Returns a free IP ID
-spec get_next_ip_id(state()) -> {state(), ip_id()}.
get_next_ip_id(#state{next_free_ip=NextID} = State ) ->
	{State#state{next_free_ip=NextID+1}, NextID}.


%% Get all IPs for a thread.
-spec thread_to_ips(integer()) -> notfound | [ip_id()].
thread_to_ips(ThreadID) ->
	case ets:lookup(thread_to_ips, ThreadID) of
		[]      -> notfound;
		Entries -> [IpID || {_Key, IpID} <- Entries]
	end.

%% Remove an IP, not caring about reverse mapping
-spec remove_ip_simple(ip_id()) -> 'true'.
remove_ip_simple(IpID) -> ets:delete(ip_to_thread, IpID).

%% Frees all IPs for a thread.
-spec free_all_ips(integer()) -> notfound | ok.
free_all_ips(ThreadID) ->
	case thread_to_ips(ThreadID) of
		notfound -> notfound;
		IpIDs ->
			lists:foreach(fun remove_ip_simple/1, IpIDs)
	end.
