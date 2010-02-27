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
%% @doc FILE fingerprint.
-module(fingFILE).
-export([load/1]).
%% The implemented functions
-export([file_fclose/3,
         file_delete/3,
         file_fgets/3,
         file_ftell/3,
         file_fopen/3,
         file_fputs/3,
         file_fread/3,
         file_fseek/3,
         file_fwrite/3]).

-include("../efunge_ip.hrl").
-include("../funge_types.hrl").

%% We use this type for our fingerprint specific data:
-type fp_file_key() :: integer().
-type fp_file() :: {IoDev::file:io_device(), BufVec::coord()}.
-type fp_file_data() :: {NextHandle::integer(), Files::dict()}.

%% Import common functions:
-import(efunge_stackstack, [push/2, pop/1, peek/1, pop_vec/1,
                            pop_gnirts/1, push_gnirts/2]).


%% @doc Load the FILE fingerprint.
-spec load(ip()) -> {ok, ip()}.
load(IP) ->
	IP2 = efunge_fingermanager:push_funs(IP, [
		{$C, fun ?MODULE:file_fclose/3},
		{$D, fun ?MODULE:file_delete/3},
		{$G, fun ?MODULE:file_fgets/3},
		{$L, fun ?MODULE:file_ftell/3},
		{$O, fun ?MODULE:file_fopen/3},
		{$P, fun ?MODULE:file_fputs/3},
		{$R, fun ?MODULE:file_fread/3},
		{$S, fun ?MODULE:file_fseek/3},
		{$W, fun ?MODULE:file_fwrite/3}]),
	{ok, IP2}.


%% The fingerprint functions

%% @spec file_fclose(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc C - Close a file
-spec file_fclose(ip(), stackstack(), fungespace()) -> return_normal().
file_fclose(IP, Stack, _Space) ->
	{S1, Handle} = pop(Stack),
	case get_file(IP, Handle) of
		error -> {efunge_ip:rev_delta(IP), S1};
		{IoDev,_} ->
			case file:close(IoDev) of
				ok -> {free_file(IP, Handle), S1};
				%% FIXME: Should we free the handle in this case?
				{error, _} -> {efunge_ip:rev_delta(IP), S1}
			end
	end.

%% @spec file_delete(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc D - Delete specified file
-spec file_delete(ip(), stackstack(), fungespace()) -> return_normal().
file_delete(IP, Stack, _Space) ->
	{S1, FileName} = pop_gnirts(Stack),
	case file:delete(FileName) of
		ok -> {IP, S1};
		{error,_} -> {efunge_ip:rev_delta(IP), S1}
	end.

%% @spec file_fgets(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc G - Get string from file (like c fgets)
-spec file_fgets(ip(), stackstack(), fungespace()) -> return_normal().
file_fgets(IP, Stack, _Space) ->
	Handle = peek(Stack),
	try
		{IoDev,_} = get_file(IP, Handle),
		{ok, Data} = file:read_line(IoDev),
		S1 = push_gnirts(Stack, Data),
		S2 = push(S1, length(Data)),
		{IP, S2}
	catch
		%% FIXME: This is quite a hack of exception handling...
		error:{badmatch,eof} -> {IP, push(push(Stack,0),0)};
		error:{badmatch,_} -> {efunge_ip:rev_delta(IP), Stack}
	end.

%% @spec file_ftell(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc L - Get current location in file
-spec file_ftell(ip(), stackstack(), fungespace()) -> return_normal().
file_ftell(IP, Stack, _Space) ->
	Handle = peek(Stack),
	try
		{IoDev,_} = get_file(IP, Handle),
		{ok, Position} = file:position(IoDev, cur),
		{IP, push(Stack, Position)}
	catch
		error:{badmatch,_} -> {efunge_ip:rev_delta(IP), Stack}
	end.

%% @spec file_fopen(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc O - Open a file (Va = i/o buffer vector)
-spec file_fopen(ip(), stackstack(), fungespace()) -> return_normal().
file_fopen(IP, Stack, _Space) ->
	{S1, Filename} = pop_gnirts(Stack),
	{S2, ModeNumber} = pop(S1),
	{S3, Vect} = pop_vec(S2),
	try
		Modes = [raw,read_ahead|map_mode(ModeNumber)],
		case file:open(Filename, Modes) of
			{ok, IoDev} ->
				fixup_file(ModeNumber, IoDev),
				{IP2, Handle} = allocate_file(IP, {IoDev, Vect}),
				{IP2, push(S3, Handle)};
			{error, _} ->
				{efunge_ip:rev_delta(IP), S3}
		end
	catch
		{efunge_file,_} -> {efunge_ip:rev_delta(IP), S3}
	end.

%% @spec file_fputs(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc P - Put string to file (like c fputs)
-spec file_fputs(ip(), stackstack(), fungespace()) -> return_normal().
file_fputs(IP, Stack, _Space) ->
	{S1, String} = pop_gnirts(Stack),
	% FIXME: Negative values
	InRangeString = [X rem 256 || X <- String],
	Handle = peek(S1),
	try
		{IoDev,_} = get_file(IP, Handle),
		ok = file:write(IoDev, InRangeString),
		{IP, S1}
	catch
		error:{badmatch,_} -> {efunge_ip:rev_delta(IP), S1}
	end.

%% @spec file_fread(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc R - Read n bytes from file to i/o buffer
-spec file_fread(ip(), stackstack(), fungespace()) -> return_normal().
file_fread(IP, Stack, Space) ->
	{S1, Bytes} = pop(Stack),
	Handle = peek(S1),
	try
		true = Bytes >= 0,
		{IoDev,{BufX,BufY}} = get_file(IP, Handle),
		{ok,Data} = file:read(IoDev,Bytes),
		write_buffer(Data,BufX,BufY,Space),
		{IP, S1}
	catch
		error:{badmatch,_} -> {efunge_ip:rev_delta(IP), S1}
	end.

%% @spec file_fseek(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc S - Seek to position in file
-spec file_fseek(ip(), stackstack(), fungespace()) -> return_normal().
file_fseek(IP, Stack, _Space) ->
	{S1, Pos} = pop(Stack),
	{S2, Operation} = pop(S1),
	Handle = peek(S2),
	try
		{IoDev,_} = get_file(IP, Handle),
		Seek =
			case Operation of
				0 -> {bof, Pos};
				1 -> {cur, Pos};
				2 -> {eof, Pos};
				_ -> throw({efunge_file, invalid_operation})
			end,
		{ok,_} = file:position(IoDev, Seek),
		{IP, S2}
	catch
		{efunge_file,_}   -> {efunge_ip:rev_delta(IP), S2};
		error:{badmatch,_} -> {efunge_ip:rev_delta(IP), S2}
	end.

%% @spec file_fwrite(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc W - Write n bytes from i/o buffer to file
-spec file_fwrite(ip(), stackstack(), fungespace()) -> return_normal().
file_fwrite(IP, Stack, Space) ->
	{S1, Bytes} = pop(Stack),
	Handle = peek(S1),
	try
		true = Bytes >= 0,
		{IoDev,{BufX,BufY}} = get_file(IP, Handle),
		Data = read_buffer([],BufX-1,BufX+Bytes-1,BufY,Space),
		ok = file:write(IoDev,Data),
		{IP, S1}
	catch
		error:{badmatch,_} -> {efunge_ip:rev_delta(IP), S1}
	end.


%% Private funtions

-spec get_data(ip()) -> fp_file_data().
get_data(IP) ->
	case efunge_fingermanager:get_data('FILE', IP) of
		{ok, Data} -> Data;
		error -> {1, dict:new()}
	end.

-spec write_data(ip(),fp_file_data()) -> ip().
write_data(IP, Data) ->
	efunge_fingermanager:set_data('FILE', Data, IP).

-spec allocate_file(ip(),fp_file()) -> {ip(),fp_file_key()}.
allocate_file(IP, FileData) ->
	{NextHandle, Files} = get_data(IP),
	Files2 = dict:store(NextHandle, FileData, Files),
	IP2 = write_data(IP, {NextHandle+1, Files2}),
	{IP2, NextHandle}.

-spec free_file(ip(),fp_file_key()) -> ip().
free_file(IP, Handle) ->
	{NextHandle, Files} = get_data(IP),
	Files2 = dict:erase(Handle, Files),
	write_data(IP, {NextHandle, Files2}).


-spec get_file(ip(),fp_file_key()) -> fp_file() | error.
get_file(IP, Handle) ->
	{_, Files} = get_data(IP),
	case dict:find(Handle, Files) of
		error -> error;
		{ok, File} -> File
	end.


%% @doc Map the mode numbers to modes.
-spec map_mode(0..5) -> ['append' | 'read' | 'write',...].
map_mode(0) -> [read];        % r
map_mode(1) -> [write];       % w
map_mode(2) -> [append];      % a
map_mode(3) -> [read,write];  % r+
map_mode(4) -> [read,write];  % w+
map_mode(5) -> [read,append]; % a+
map_mode(_N) -> throw({efunge_file,invalid_mode}).


%% @doc Do post-cleanup to emulate certain modes.
-spec fixup_file(0..5,file:io_device()) -> 'ok'.
fixup_file(2, IoDev) -> {ok, 0} = file:position(IoDev, bof), ok;
fixup_file(4, IoDev) -> ok = file:truncate(IoDev);
fixup_file(5, IoDev) -> {ok, 0} = file:position(IoDev, bof), ok;
fixup_file(N, _IoDev) when is_integer(N) -> ok.

%% @doc
%% Write a list into the buffer in funge-space.
-spec write_buffer(string(),integer(),integer(),fungespace()) -> 'ok'.
write_buffer([], _X, _Y, _Space) ->
	ok;
write_buffer([H|T], X, Y, Space) ->
	efunge_fungespace:set(Space, {X,Y},H),
	write_buffer(T, X+1, Y, Space).

%% @doc
%% Read buffer into a list.
-spec read_buffer([cell()],integer(),integer(),integer(),fungespace()) -> [integer()].
read_buffer(Data, MinX, MinX, _Y, _Space) ->
	Data;
read_buffer(Data, MinX, X, Y, Space) ->
	Value = efunge_fungespace:fetch(Space, {X,Y}),
	% FIXME: negative values!
	read_buffer([Value rem 256|Data], MinX, X-1, Y, Space).
