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
%% @doc This server handles input buffer and extracting values from said
%% buffers.
-module(efunge_input).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, stop/0]).
-export([read_next_char/0, read_next_integer/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {stringBuffer = [] :: list()}).

-type state() :: #state{}.

-type gen_server_start_error() :: {error,{already_started, pid()} | any()}.
-type gen_server_start() :: {ok,pid()} | ignore | gen_server_start_error().

-type call_return_reply() :: {reply, eof | integer() | char(),#state{}}.
-type call_return_stop()  :: {stop,normal,stopped,#state{}}.
-type call_return()       :: call_return_reply() | call_return_stop().

%%====================================================================
%% API
%%====================================================================

%% @spec start() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server, standalone
-spec start() -> gen_server_start().
start() ->
	gen_server:start({global, ?SERVER}, ?MODULE, [], []).

%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server, linked to supervisor.
-spec start_link() -> gen_server_start().
start_link() ->
	gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

-spec stop() -> stopped.
stop() ->
	gen_server:call({global, ?SERVER}, stop, infinity).

%% @spec read_next_char() -> eof | char()
%% @doc Get a letter from the string buffer.
-spec read_next_char() -> eof | char().
read_next_char() ->
	gen_server:call({global, ?SERVER}, read_char, infinity).

%% @spec read_next_integer() -> eof | integer()
%% @doc Get an integer from the string buffer.
-spec read_next_integer() -> eof | integer().
read_next_integer() ->
	gen_server:call({global, ?SERVER}, read_integer, infinity).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%% @hidden
%% @doc Initiates the server
-spec init([]) -> {'ok',#state{}}.
init([]) ->
	{ok, #state{}}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} | {stop, Reason, Reply, State}
%% @hidden
%% @doc Handling call messages
-spec handle_call(read_char | read_integer | stop,_,#state{}) -> call_return().
handle_call(read_char, _From, State) ->
	{NewState, Reply} = read_next_char(State),
	{reply, Reply, NewState};
handle_call(read_integer, _From, State) ->
	{NewState, Reply} = read_next_integer(State),
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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @spec fill_buffer(state()) -> {ok, NewState::ip()} | {eof, NewState::state()}
%% @doc Fill up the input line buffer if needed.
-spec fill_buffer(state()) -> {ok, state()} | {eof, state()}.
fill_buffer(#state{} = State) ->
	if
		State#state.stringBuffer =:= [] ->
			String = io:get_line(''),
			if
				String =:= eof -> {eof, State};
				true ->
					{ok, State#state{ stringBuffer=String }}
			end;
		true ->
			{ok, State}
	end.

%% @spec read_next_char(state()) -> {NewState, Char}
%% @doc Get a letter from the string buffer.
-spec read_next_char(state()) -> {state(), char() | eof}.
read_next_char(#state{} = State) ->
	case fill_buffer(State) of
		{eof, _} -> {State, eof};
		{ok, NewState} ->
			StringBuf = NewState#state.stringBuffer,
			[H|T] = StringBuf,
			{NewState#state{ stringBuffer=T }, H}
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
				_ -> Result
			end
	end.


%% @spec read_next_integer(state()) -> {NewState::state(), eof | integer()}
%% @doc Get an integer from the string buffer.
-spec read_next_integer(state()) -> {state(), eof | integer()}.
read_next_integer(#state{} = State) ->
	case fill_buffer(State) of
		{eof, _} -> {State, eof};
		{ok, NewState} ->
			case parse_integer(NewState#state.stringBuffer) of
				%% Try again!
				error ->
					read_next_integer(NewState#state{ stringBuffer=[] });
				{Int, Rest} ->
					{NewState#state{ stringBuffer=Rest }, Int}
			end
	end.
