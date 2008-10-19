%%%-------------------------------------------------------------------
%%% File    : efunge_id_server.erl
%%% Author  : Arvid Norlander <arvid@tux.lan>
%%% Description :
%%%
%%% Created : 18 Oct 2008 by Arvid Norlander <arvid@tux.lan>
%%%-------------------------------------------------------------------
-module(efunge_id_server).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, stop/0]).
-export([alloc_thread_id/0, alloc_ip_id/1,
         free_thread_id/1, free_ip_id/1,
         lookup_thread/1, lookup_ip_pid/1, lookup_ip_thread/1]).

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

-record(state,
	{
		next_free_thread = 0 :: integer(),
		next_free_ip = 0 :: integer()
	}
).

-type state()     :: #state{}.
-type thread_id() :: integer().
-type ip_id()     :: integer().

-type call_return_reply() :: {reply, notfound | ok | pid() | thread_id() | ip_id(), state()}.
-type call_return_stop()  :: {stop,normal,stopped,state()}.
-type call_return()       :: call_return_reply() | call_return_stop().
-type call_op_none() :: alloc_thread_id | stop.
-type call_op_int()  :: alloc_ip_id | free_ip_id | free_thread_id | lookup_ip_pid | lookup_ip_thread | lookup_thread.
-type call_op() :: call_op_none() | {call_op_int(), thread_id() | ip_id()}.

-include("otp_types.hrl").

%%====================================================================
%% API
%%====================================================================

%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server, linked to supervisor.
-spec start_link() -> otp_start_return().
start_link() ->
	gen_server:start_link(?REGISTER_NAME, ?MODULE, [], []).

%% @spec start() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server, standalone.
-spec start() -> otp_start_return().
start() ->
	gen_server:start(?REGISTER_NAME, ?MODULE, [], []).

%% @spec stop() -> stopped
%% @doc Stops the server. Use only for standalone server.
-spec stop() -> stopped.
stop() ->
	gen_server:call(?CALL_NAME, stop).

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

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec create_ets_tables() -> ok.
create_ets_tables() ->
	ets:new(thread_to_pid, [set, private, named_table]),
	ets:new(ip_to_thread, [set, private, named_table]),
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
		[]                -> notfound;
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
