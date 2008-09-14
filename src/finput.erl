%% @doc Handles input buffer stuff.
-module(finput).
-export([readNextChar/1, readNextInteger/1]).
-include("fip.hrl").
-include("funge_types.hrl").

%% @spec fillBuffer(ip()) -> {ok, NewState::ip()} | {eof, NewState::ip()}
%% @doc Fill up the input line buffer if needed.
-spec fillBuffer(ip()) -> {ok, ip()} | {eof, ip()}.
fillBuffer(#fip{} = State) ->
	StringBuf = State#fip.stringBuffer,
	if
		StringBuf =:= [] ->
			String = io:get_line(''),
			if
				String =:= eof -> {eof, State};
				true ->
					{ok, State#fip{ stringBuffer=String }}
			end;
		true ->
			{ok, State}
	end.

%% @spec readNextChar(ip()) -> {NewState, Char}
%% @doc Get a letter from the string buffer.
-spec readNextChar(ip()) -> {ip(), char() | eof}.
readNextChar(#fip{} = State) ->
	{Status, NewState} = fillBuffer(State),
	case Status of
		eof -> {State, eof};
		ok ->
			StringBuf = NewState#fip.stringBuffer,
			[H|T] = StringBuf,
			{NewState#fip{ stringBuffer=T }, H}
	end.

%% @spec parseInteger(string()) -> {integer(), Rest::string()} | error
%% @doc Parse an integer in a string, return what is left after the end of the
%%      integer, discarding a newlines if there is one directly after the integer.
-spec parseInteger(string()) -> 'error' | {integer(),string()}.
parseInteger([]) -> error;
parseInteger(String) ->
	Result = string:to_integer(String),
	case Result of
		{error, _Reason} ->
			[_H|T] = String,
			parseInteger(T);
		{Int, Rest} ->
			[H|T] = Rest,
			case H of
				$\n -> {Int, T};
				_ -> Result
			end
	end.


%% @spec readNextInteger(ip()) -> {NewState::ip(), eof | integer()}
%% @doc Get an integer from the string buffer.
-spec readNextInteger(ip()) -> {ip(), eof | integer()}.
readNextInteger(#fip{} = State) ->
	{Status, NewState} = fillBuffer(State),
	case Status of
		eof -> {State, eof};
		ok ->
			StringBuf = NewState#fip.stringBuffer,
			Result = parseInteger(StringBuf),
			case Result of
				%% Try again!
				error ->
					readNextInteger(NewState#fip{ stringBuffer=[] });
				{Int, Rest} ->
					{NewState#fip{ stringBuffer=Rest }, Int}
			end
	end.
