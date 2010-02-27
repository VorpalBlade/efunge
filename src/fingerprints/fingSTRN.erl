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
%% @doc STRN fingerprint.
-module(fingSTRN).
-export([load/1]).
%% The implemented functions
-export([
         strn_append/3,
         strn_compare/3,
         strn_display/3,
         strn_search/3,
         strn_get/3,
         strn_input/3,
         strn_left/3,
         strn_slice/3,
         strn_length/3,
         strn_put/3,
         strn_right/3,
         strn_itoa/3,
         strn_atoi/3]).

-include("../efunge_ip.hrl").
-include("../funge_types.hrl").

%% Import common functions:
-import(efunge_stackstack, [push/2, pop/1, pop_vec/1,
                            pop_gnirts/1, push_gnirts/2]).


%% @doc Load the STRN fingerprint.
-spec load(ip()) -> {ok, ip()}.
load(IP) ->
	IP2 = efunge_fingermanager:push_funs(IP, [
		{$A, fun ?MODULE:strn_append/3},
		{$C, fun ?MODULE:strn_compare/3},
		{$D, fun ?MODULE:strn_display/3},
		{$F, fun ?MODULE:strn_search/3},
		{$G, fun ?MODULE:strn_get/3},
		{$I, fun ?MODULE:strn_input/3},
		{$L, fun ?MODULE:strn_left/3},
		{$M, fun ?MODULE:strn_slice/3},
		{$N, fun ?MODULE:strn_length/3},
		{$P, fun ?MODULE:strn_put/3},
		{$R, fun ?MODULE:strn_right/3},
		{$S, fun ?MODULE:strn_itoa/3},
		{$V, fun ?MODULE:strn_atoi/3}]),
	{ok, IP2}.


%% The fingerprint functions

%% @spec strn_append(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc A - Append bottom string to upper string
-spec strn_append(ip(), stackstack(), fungespace()) -> return_normal().
strn_append(IP, Stack, _Space) ->
	{S1, Top} = pop_gnirts(Stack),
	{S2, Bottom} = pop_gnirts(S1),
	{IP, push_gnirts(S2, Top ++ Bottom)}.

%% @spec strn_compare(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc C - Compare strings
-spec strn_compare(ip(), stackstack(), fungespace()) -> return_normal().
strn_compare(IP, Stack, _Space) ->
	{S1, A} = pop_gnirts(Stack),
	{S2, B} = pop_gnirts(S1),
	if
		A < B -> {IP, push(S2, -1)};
		A > B -> {IP, push(S2, 1)};
		true  -> {IP, push(S2, 0)}
	end.

%% @spec strn_display(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc D - Display a string
-spec strn_display(ip(), stackstack(), fungespace()) -> return_normal().
strn_display(IP, Stack, _Space) ->
	{S1, Str} = pop_gnirts(Stack),
	io:put_chars(Str),
	{IP, S1}.

%% @spec strn_search(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc F - Search for bottom string in upper string
-spec strn_search(ip(), stackstack(), fungespace()) -> return_normal().
strn_search(IP, Stack, _Space) ->
	{S1, Top} = pop_gnirts(Stack),
	{S2, Bottom} = pop_gnirts(S1),
	case string:str(Top, Bottom) of
		0   -> {IP, push(S2, 0)};
		Idx -> {IP, push_gnirts(S2, string:substr(Top,Idx))}
	end.

%% @spec strn_get(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc G - Get string from specified position
-spec strn_get(ip(), stackstack(), fungespace()) -> return_normal().
strn_get(IP=#fip{offX = OffX, offY = OffY}, Stack, Space) ->
	{{MinX,MinY},{MaxX,MaxY}} = efunge_fungespace:get_bounds(Space),
	{S1, {RX,RY}} = pop_vec(Stack),
	X = RX + OffX,
	Y = RY + OffY,
	if
		Y < MinY; Y > MaxY; X < MinX; X > MaxX ->
			{efunge_ip:rev_delta(IP), S1};
		true ->
			Str = get_fspace_str([], X, MaxX, Y, Space),
			{IP, push_gnirts(S1, Str)}
	end.

%% @spec strn_input(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc I - Input a string
-spec strn_input(ip(), stackstack(), fungespace()) -> return_normal().
strn_input(IP, Stack, _Space) ->
	case efunge_input:read_line() of
		eof -> {efunge_ip:rev_delta(IP), Stack};
		error -> {efunge_ip:rev_delta(IP), Stack};
		String ->
			%% Somewhat inefficient this... But since we are dealing with
			%% a linked list basically...
			[H|T] = lists:reverse(String),
			case H of
				$\n -> {IP, push_gnirts(Stack,lists:reverse(T))};
				_ -> {IP, push_gnirts(Stack,String)}
			end
	end.

%% @spec strn_left(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc L - Leftmost n characters of string
-spec strn_left(ip(), stackstack(), fungespace()) -> return_normal().
strn_left(IP, Stack, _Space) ->
	{S1, N} = pop(Stack),
	{S2, Str} = pop_gnirts(S1),
	if
		N < 0; N > length(Str) ->
			{efunge_ip:rev_delta(IP), S2};
		true ->
			{IP, push_gnirts(S2, string:substr(Str, 1, N))}
	end.

%% @spec strn_slice(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc M - n characters starting at position s
-spec strn_slice(ip(), stackstack(), fungespace()) -> return_normal().
strn_slice(IP, Stack, _Space) ->
	{S1, N} = pop(Stack),
	{S2, P} = pop(S1),
	{S3, Str} = pop_gnirts(S2),
	if
		N < 0; P < 0; P+N > length(Str) ->
			{efunge_ip:rev_delta(IP), S3};
		true ->
			{IP, push_gnirts(S3, string:substr(Str, P+1, N))}
	end.

%% @spec strn_length(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc N - Get length of string
-spec strn_length(ip(), stackstack(), fungespace()) -> return_normal().
strn_length(IP, Stack, _Space) ->
	{_, Str} = pop_gnirts(Stack),
	{IP, push(Stack, length(Str))}.

%% @spec strn_put(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc P - Put string at specified position
-spec strn_put(ip(), stackstack(), fungespace()) -> return_normal().
strn_put(IP=#fip{offX = OffX, offY = OffY}, Stack, Space) ->
	{S1, {RX,RY}} = pop_vec(Stack),
	X = RX + OffX,
	Y = RY + OffY,
	S2 = write_fspace_str(S1, X, Y, Space),
	{IP, S2}.

%% @spec strn_right(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc R - Rightmost n characters of string
-spec strn_right(ip(), stackstack(), fungespace()) -> return_normal().
strn_right(IP, Stack, _Space) ->
	{S1, N} = pop(Stack),
	{S2, Str} = pop_gnirts(S1),
	L = length(Str),
	if
		N < 0; N > L ->
			{efunge_ip:rev_delta(IP), S2};
		true ->
			{IP, push_gnirts(S2, string:substr(Str, L-N+1))}
	end.

%% @spec strn_itoa(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc S - String representation of a number
-spec strn_itoa(ip(), stackstack(), fungespace()) -> return_normal().
strn_itoa(IP, Stack, _Space) ->
	{S1, N} = pop(Stack),
	{IP, push_gnirts(S1,integer_to_list(N))}.

%% @spec strn_atoi(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc V - Retrieve value from string
-spec strn_atoi(ip(), stackstack(), fungespace()) -> return_normal().
strn_atoi(IP, Stack, _Space) ->
	{S1, Str} = pop_gnirts(Stack),
	case string:to_integer(Str) of
		{error,no_integer} -> {IP, push(S1,0)};
		{Int,_} -> {IP, push(S1,Int)}
	end.


%% Private funtions
%% @doc Read a string from funge-space.
-spec get_fspace_str(String::[cell()],X::cell(),MaxX::cell(),Y::cell(),fungespace()) -> [cell()].
get_fspace_str(Str, MaxX, MaxX, _Y, _Space) ->
	lists:reverse(Str);
get_fspace_str(Str, X, MaxX, Y, Space) ->
	case efunge_fungespace:fetch(Space, {X,Y}) of
		0 -> lists:reverse(Str);
		V -> get_fspace_str([V|Str], X+1, MaxX, Y, Space)
	end.

%% @doc Write a string from the stack to funge-space.
-spec write_fspace_str(stackstack(),X::cell(),Y::cell(),fungespace()) -> stackstack().
write_fspace_str(Stack, X, Y, Space) ->
	{S1, Value} = pop(Stack),
	efunge_fungespace:set(Space, {X,Y}, Value),
	if
		Value =:= 0 -> S1;
		true -> write_fspace_str(S1, X+1, Y, Space)
	end.
