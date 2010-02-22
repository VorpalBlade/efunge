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
%% @doc ATHR fingerprint.
-module(fingATHR).
-export([load/1]).
%% The implemented functions
-export([athr_borrow/3,
         athr_cas/3,
         athr_flush/3,
         athr_sget/3,
         athr_id/3,
         athr_signal/3,
         athr_broadcast/3,
         athr_sput/3,
         athr_quit/3,
         athr_return/3,
         athr_spawn/3,
         athr_try_borrow/3,
         athr_wait/3]).

-include("../efunge_ip.hrl").
-include("../funge_types.hrl").

%% Import common functions:
-import(efunge_stackstack, [push/2, pop/1, pop_vec/1, push_list/2]).


-define(TIMEOUT_TO_MILLISECS,1000000).

%% @doc Load the ATHR fingerprint.
-spec load(ip()) -> {ok, ip()}.
load(IP) ->
	IP2 = efunge_fingermanager:push_funs(IP, [
		{$B, fun ?MODULE:athr_borrow/3},
		{$C, fun ?MODULE:athr_cas/3},
		{$F, fun ?MODULE:athr_flush/3},
		{$G, fun ?MODULE:athr_sget/3},
		{$I, fun ?MODULE:athr_id/3},
		{$N, fun ?MODULE:athr_signal/3},
		{$O, fun ?MODULE:athr_broadcast/3},
		{$P, fun ?MODULE:athr_sput/3},
		{$Q, fun ?MODULE:athr_quit/3},
		{$R, fun ?MODULE:athr_return/3},
		{$S, fun ?MODULE:athr_spawn/3},
		{$T, fun ?MODULE:athr_try_borrow/3},
		{$W, fun ?MODULE:athr_wait/3}]),
	{ok, IP2}.


%% The fingerprint functions

%% @spec athr_borrow(ip(), stackstack(), fungespace()) -> execute_return()
%% @doc B - Borrow book
-spec athr_borrow(ip(), stackstack(), fungespace()) -> execute_return().
athr_borrow(IP, Stack, Space) ->
	{efunge_ip:rev_delta(IP), Stack}.

%% @spec athr_cas(ip(), stackstack(), fungespace()) -> execute_return()
%% @doc C - Compare and exchange
-spec athr_cas(ip(), stackstack(), fungespace()) -> execute_return().
athr_cas(IP, Stack, Space) ->
	{S1, C} = pop_vec(Stack),
	{S2, Old} = pop(S1),
	{S3, New} = pop(S2),
	{_, RealOld} = efunge_fungespace:cmpxchg(Space, IP, C, Old, New),
	{IP, push(S3, RealOld)}.

%% @spec athr_flush(ip(), stackstack(), fungespace()) -> execute_return()
%% @doc F - Flushes the signal queue
-spec athr_flush(ip(), stackstack(), fungespace()) -> execute_return().
athr_flush(IP, Stack, _Space) ->
	{Len, SigList} = get_all_wait_sigs(),
	S1 = push_list(Stack, SigList),
	{IP, push(S1, Len)}.

%% @spec athr_sget(ip(), stackstack(), fungespace()) -> execute_return()
%% @doc G - Synchronous get
-spec athr_sget(ip(), stackstack(), fungespace()) -> execute_return().
athr_sget(IP, Stack, Space) ->
	{S1, C} = pop_vec(Stack),
	V = efunge_fungespace:fetch_atomic(Space, IP, C),
	{IP, push(S1, V)}.

%% @spec athr_id(ip(), stackstack(), fungespace()) -> execute_return()
%% @doc I - ID of current thread
-spec athr_id(ip(), stackstack(), fungespace()) -> execute_return().
athr_id(#fip{threadID=ThID} = IP, Stack, _Space) ->
	{IP, push(Stack, ThID)}.

%% @spec athr_signal(ip(), stackstack(), fungespace()) -> execute_return()
%% @doc N - Send signal
-spec athr_signal(ip(), stackstack(), fungespace()) -> execute_return().
athr_signal(IP, Stack, _Space) ->
	{S1, ThID} = pop(Stack),
	{S2, SigID} = pop(S1),
	case efunge_id_server:lookup_thread(ThID) of
		notfound ->
			{efunge_ip:rev_delta(IP), S2};
		Pid ->
			Pid ! {athr_wait_sig, SigID}, {IP, S2}
	end.

%% @spec athr_broadcast(ip(), stackstack(), fungespace()) -> execute_return()
%% @doc O - Broadcast signal
-spec athr_broadcast(ip(), stackstack(), fungespace()) -> execute_return().
athr_broadcast(IP, Stack, _Space) ->
	{S1, SigID} = pop(Stack),
	efunge_supervisor_threads:cast_athr_signal(SigID),
	{IP, S1}.

%% @spec athr_sput(ip(), stackstack(), fungespace()) -> execute_return()
%% @doc P - Synchronous put
-spec athr_sput(ip(), stackstack(), fungespace()) -> execute_return().
athr_sput(IP, Stack, Space) ->
	{S1, C} = pop_vec(Stack),
	{S2, V} = pop(S1),
	efunge_fungespace:set_atomic(Space, IP, C, V),
	{IP, S2}.

%% @spec athr_quit(ip(), stackstack(), fungespace()) -> execute_return()
%% @doc Q - Quit
-spec athr_quit(ip(), stackstack(), fungespace()) -> execute_return().
athr_quit(_IP, _Stack, _Space) ->
	{dead, {athr_quit, 0}}.

%% @spec athr_return(ip(), stackstack(), fungespace()) -> execute_return()
%% @doc R - Return book
-spec athr_return(ip(), stackstack(), fungespace()) -> execute_return().
athr_return(IP, Stack, Space) ->
	{efunge_ip:rev_delta(IP), Stack}.

%% @spec athr_spawn(ip(), stackstack(), fungespace()) -> execute_return()
%% @doc S - Spawn thread
-spec athr_spawn(ip(), stackstack(), fungespace()) -> execute_return().
athr_spawn(IP, Stack, Space) ->
	ChildIP = efunge_ip:ip_forward(efunge_ip:rev_delta(IP)),
	case efunge_supervisor_threads:create_thread(Space, ChildIP, Stack) of
		{ok, _Pid, _ThreadID} -> {IP, Stack};
		{error, _Reason} -> {efunge_ip:rev_delta(IP), Stack}
	end.

%% @spec athr_try_borrow(ip(), stackstack(), fungespace()) -> execute_return()
%% @doc T - Try to borrow
-spec athr_try_borrow(ip(), stackstack(), fungespace()) -> execute_return().
athr_try_borrow(IP, Stack, Space) ->
	{efunge_ip:rev_delta(IP), Stack}.

%% @spec athr_wait(ip(), stackstack(), fungespace()) -> execute_return()
%% @doc W - Wait for signal
-spec athr_wait(ip(), stackstack(), fungespace()) -> execute_return().
athr_wait(IP, Stack, _Space) ->
	{S1, SigID} = pop(Stack),
	{S2, TimeoutVal} = pop(S1),
	Parent = get(efunge_thread_parent),
	Timeout =
		if
			TimeoutVal =:= -1 -> infinity;
			TimeoutVal < -1 -> 0;
			%% Convert to milliseconds.
			true ->
				%% Round upwards.
				(TimeoutVal+?TIMEOUT_TO_MILLISECS-1) div ?TIMEOUT_TO_MILLISECS
		end,
	%% NOTE: Maybe we should instead handle this in efunge_thread?
	%% TODO: Do we need we handle system messages? If so, how in here?
	%% Note that we need to handle exit signals from the supervisor here too, or
	%% there might be issues with shutting down on q.
	receive
		{athr_wait_sig, SigID} -> {IP, S2};
		{'EXIT', Parent, Reason} ->
			%% TODO? Pass the exit handling up somewhere or?
			exit(Reason)
	after Timeout ->
		{efunge_ip:rev_delta(IP), S2}
	end.


%% Private funtions
-spec get_all_wait_sigs() -> {Length::non_neg_integer(),Signals::[integer()]}.
get_all_wait_sigs() ->
	get_all_wait_sigs([], 0).

-spec get_all_wait_sigs([integer()],non_neg_integer())
   -> {Length::non_neg_integer(),Signals::[integer()]}.
get_all_wait_sigs(SigList, Len) ->
	receive
		{athr_wait_sig, SigID} ->
			get_all_wait_sigs([SigID|SigList], Len+1)
	after 0 ->
		{Len, SigList}
	end.
