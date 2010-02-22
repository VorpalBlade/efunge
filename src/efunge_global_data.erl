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
%% @doc This server handles global state for core.
-module(efunge_global_data).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, stop/0]).
-export([set_cmdline/1, get_cmdline/0]).


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
		command_line = [] :: list(string())
	}).


%%====================================================================
%% Types
%%====================================================================

%% The string buffer.
-type state() :: #state{}.

-include("otp_types.hrl").

-type call_return_reply() :: {reply, any(), state()}.
-type call_return_stop()  :: {stop,normal,stopped,state()}.
-type call_return()       :: call_return_reply() | call_return_stop().

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

-spec stop() -> stopped.
stop() ->
	gen_server:call(?CALL_NAME, stop).


%%====================================================================
%% API - Calls
%%====================================================================

-spec set_cmdline([string()]) -> ok.
set_cmdline(Commandline) ->
	gen_server:call(?CALL_NAME, {set_cmdline, Commandline}).

-spec get_cmdline() -> [string()].
get_cmdline() ->
	gen_server:call(?CALL_NAME, get_cmdline).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%% @hidden
%% @doc Initiates the server
-spec init([]) -> {ok, #state{}}.
init([]) ->
	{ok, #state{}}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} | {stop, Reason, Reply, State}
%% @hidden
%% @doc Handling call messages
-spec handle_call(get_cmdline | {set_cmdline, [string()]} | stop,_,state()) -> call_return().
handle_call(get_cmdline, _From, State) ->
	{reply, State#state.command_line, State};
handle_call({set_cmdline, Commandline}, _From, State) ->
	{reply, ok, State#state{command_line = Commandline}};
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
-spec terminate(_,state()) -> 'ok'.
terminate(_Reason, _State) ->
	ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @hidden
%% @doc Convert process state when code is changed
-spec code_change(_,state(),_) -> {'ok',state()}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================
