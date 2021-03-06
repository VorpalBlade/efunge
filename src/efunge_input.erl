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
%% @doc This server handles input buffer and extracting values from said
%% buffers.
-module(efunge_input).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, stop/0]).
-export([read_char/0, read_integer/0, read_line/0]).

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

%% The string buffer.
-type state() :: [] | list(integer()).
%% The possible client visible error replies.
-type error_replies() :: eof | error.

-type gen_server_start_error() :: {error,{already_started, pid()} | any()}.
-type gen_server_start() :: {ok,pid()} | ignore | gen_server_start_error().

%%====================================================================
%% API - Generic start/stop stuff
%%====================================================================

%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server, linked to supervisor.
-spec start_link() -> gen_server_start().
start_link() ->
	gen_server:start_link(?REGISTER_NAME, ?MODULE, [], []).

%% @spec start() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server, standalone.
-spec start() -> gen_server_start().
start() ->
	gen_server:start(?REGISTER_NAME, ?MODULE, [], []).

%% @spec stop() -> stopped
%% @doc Stops the server. Use only for standalone server.
-spec stop() -> stopped.
stop() ->
	gen_server:call(?CALL_NAME, stop, infinity).


%%====================================================================
%% API - Calls
%%====================================================================

%% @spec read_char() -> eof | char()
%% @doc Get a letter from the string buffer.
-spec read_char() -> error_replies() | char().
read_char() ->
	gen_server:call(?CALL_NAME, read_char, infinity).

%% @spec read_integer() -> eof | integer()
%% @doc Get an integer from the string buffer.
-spec read_integer() -> error_replies() | integer().
read_integer() ->
	gen_server:call(?CALL_NAME, read_integer, infinity).

%% @doc
%% Get an entire line. If there is anything in the buffer that is what will be
%% returned. If buffer is empty, new line will be fetched.
-spec read_line() -> error_replies() | string().
read_line() ->
	gen_server:call(?CALL_NAME, read_line, infinity).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%% @hidden
%% @doc Initiates the server
-spec init([]) -> {'ok', state()}.
init([]) ->
	{ok, []}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} | {stop, Reason, Reply, State}
%% @hidden
%% @doc Handling call messages
-spec handle_call(read_char, _, state()) -> {reply, error_replies() | char(), state()}
               ; (read_line, _, state()) -> {reply, error_replies() | string(), state()}
               ; (read_integer, _, state()) -> {reply, error_replies() | integer(), state()}
               ; (stop, _, state()) -> {stop,normal,stopped,state()}.
handle_call(read_char, _From, State) ->
	{NewState, Reply} = read_char(State),
	{reply, Reply, NewState};
handle_call(read_line, _From, State) ->
	{NewState, Reply} = read_line(State),
	{reply, Reply, NewState};
handle_call(read_integer, _From, State) ->
	{NewState, Reply} = read_integer(State),
	{reply, Reply, NewState};
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

%% @spec fill_buffer(state()) -> {ok, NewState::ip()} | {eof, NewState::state()}
%% @doc Fill up the input line buffer if needed.
-spec fill_buffer(state()) -> {ok | error, state()} | {eof, []}.
fill_buffer([]) ->
	case io:get_line('') of
		eof       -> {eof, []};
		{error,_} -> {error, []};
		String    ->
			% TODO: Maybe handle the remaining data somehow?
			case unicode:characters_to_list(String, unicode) of
				{error, StringList, _Rest}      -> {error, StringList};
				{incomplete, StringList, _Rest} -> {error, StringList};
				StringList                      -> {ok, StringList}
			end
	end;
fill_buffer(State) ->
	{ok, State}.

%% @spec read_char(state()) -> {NewState, Char}
%% @doc Get a letter from the string buffer.
-spec read_char(state()) -> {state(), char() | error_replies()}.
read_char(State) ->
	{Result, NewState} = fill_buffer(State),
	case Result of
		eof   -> {NewState, eof};
		% TODO: Not sure this is right way to handle it.
		error -> {NewState, error};
		ok ->
			[H|T] = NewState,
			{T, H}
	end.



%% @doc Get an entire line.
-spec read_line(state()) -> {state(), string() | error_replies()}.
read_line(State) ->
	{Result, NewState} = fill_buffer(State),
	case Result of
		eof   -> {NewState, eof};
		error -> {NewState, error};
		ok    -> {[], NewState}
	end.

%% @spec parse_integer(string()) -> {integer(), Rest::string()} | error
%% @doc Parse an integer in a string, return what is left after the end of the
%%      integer, discarding a newlines if there is one directly after the integer.
-spec parse_integer(string()) -> error | {integer(),string()}.
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
				_   -> Result
			end
	end.

%% @spec read_integer(state()) -> {NewState::state(), eof | error | integer()}
%% @doc Get an integer from the string buffer.
-spec read_integer(state()) -> {state(), integer() | error_replies()}.
read_integer(State) ->
	{Result, NewState} = fill_buffer(State),
	case Result of
		eof   -> {NewState, eof};
		error -> {NewState, error};
		ok ->
			case parse_integer(NewState) of
				%% Try again!
				error ->
					read_integer([]);
				{Int, Rest} ->
					{Rest, Int}
			end
	end.
