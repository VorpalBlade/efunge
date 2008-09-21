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
%% @doc This module handles input buffer and extracting values from said
%% buffers.
-module(finput).
-export([read_next_char/1, read_next_integer/1]).
-include("fip.hrl").
-include("funge_types.hrl").

%% @spec fill_buffer(ip()) -> {ok, NewState::ip()} | {eof, NewState::ip()}
%% @doc Fill up the input line buffer if needed.
-spec fill_buffer(ip()) -> {ok, ip()} | {eof, ip()}.
fill_buffer(#fip{} = State) ->
	if
		State#fip.stringBuffer =:= [] ->
			String = io:get_line(''),
			if
				String =:= eof -> {eof, State};
				true ->
					{ok, State#fip{ stringBuffer=String }}
			end;
		true ->
			{ok, State}
	end.

%% @spec read_next_char(ip()) -> {NewState, Char}
%% @doc Get a letter from the string buffer.
-spec read_next_char(ip()) -> {ip(), char() | eof}.
read_next_char(#fip{} = State) ->
	case fill_buffer(State) of
		{eof, _} -> {State, eof};
		{ok, NewState} ->
			StringBuf = NewState#fip.stringBuffer,
			[H|T] = StringBuf,
			{NewState#fip{ stringBuffer=T }, H}
	end.

%% @spec parse_integer(string()) -> {integer(), Rest::string()} | error
%% @doc Parse an integer in a string, return what is left after the end of the
%%      integer, discarding a newlines if there is one directly after the integer.
-spec parse_integer(string()) -> 'error' | {integer(),string()}.
parse_integer([]) -> error;
parse_integer(String) ->
	Result = string:to_integer(String),
	case Result of
		{error, _Reason} ->
			[_H|T] = String,
			parse_integer(T);
		{Int, Rest} ->
			[H|T] = Rest,
			case H of
				$\n -> {Int, T};
				_ -> Result
			end
	end.


%% @spec read_next_integer(ip()) -> {NewState::ip(), eof | integer()}
%% @doc Get an integer from the string buffer.
-spec read_next_integer(ip()) -> {ip(), eof | integer()}.
read_next_integer(#fip{} = State) ->
	case fill_buffer(State) of
		{eof, _} -> {State, eof};
		{ok, NewState} ->
			case parse_integer(NewState#fip.stringBuffer) of
				%% Try again!
				error ->
					read_next_integer(NewState#fip{ stringBuffer=[] });
				{Int, Rest} ->
					{NewState#fip{ stringBuffer=Rest }, Int}
			end
	end.
