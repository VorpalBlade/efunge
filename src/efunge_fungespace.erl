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
%% @doc This module handles the Funge Space.
%%
%% It is a server for Funge-Space ets table (public) as well as handles
%% G/P/C from ATHR.
%%
%% Format of tuples in table is {{X,Y},Value}. The current implementation use
%% ETS tables, but that may change without prior notice.
%%
%% The current implementation also use a special key to store the bounds of the
%% Funge-Space.
-module(efunge_fungespace).

-behaviour(gen_server).

%% API - OTP stuff
-export([start/0, start_link/0, stop/0]).
%% API - Calls to server.
-export([get_fungespace/0, set_atomic/3, fetch_atomic/2, cmpxchg/4]).

%% API - Non-calls
-export([load_initial/2, set/3, set/4, load/5,
         fetch/2, fetch/3, get_block/3,
         get_bounds_thread/0, get_bounds_thread_exact/1, get_bounds/1]).

%% API - Utility functions
-export([is_in_range/2, set_process_bounds_initial/1]).

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

-include("efunge_ip.hrl").
-include("funge_types.hrl").
%% @headerfile "efunge_ip.hrl"

%% @type coord() = {X::integer(), Y::integer()}.
%%   Funge Space coordinates.
%% @type fungespace().
%%   A Funge Space. The actual type is internal.

-type integer_or_undef() :: undefined | integer().

%% The state, the funge-space ID.
-type state() :: fungespace().

-include("otp_types.hrl").

-type load_initial_return() :: notfound | ok.
-type cmpxchg_return()      :: {ok, integer()} | {failed, integer()}.
-type call_return_replies() :: true | fungespace() | integer() | cmpxchg_return() | load_initial_return().
-type call_return_reply() :: {reply, call_return_replies(), state()}.
-type call_return_stop()  :: {stop,normal,stopped,state()}.
-type call_return()       :: call_return_reply() | call_return_stop().

-type arg_fetch_atomic() :: {fetch_atomic,fungespace(),coord()}.
-type arg_load_initial() :: {load_initial,fungespace(),string()}.
-type arg_set_atomic()   :: {set_atomic,fungespace(),coord(),integer()}.
-type arg_cmpxchg()      :: {cmpxchg,fungespace(),coord(),integer(),integer()}.
-type call_arg() :: get_fungespace|stop|arg_fetch_atomic()|arg_load_initial()|arg_set_atomic()|arg_cmpxchg().


%%====================================================================
%% API - Generic start/stop stuff
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


%%====================================================================
%% API - Calls
%%====================================================================

%% @doc Only call this to initially load the file.
-spec get_fungespace() -> fungespace().
get_fungespace() ->
	gen_server:call(?CALL_NAME, get_fungespace).

-spec load_initial(fungespace(), string()) -> load_initial_return().
load_initial(Fungespace, Filename) when is_list(Filename) ->
	gen_server:call(?CALL_NAME, {load_initial, Fungespace, Filename}).

-spec set_atomic(fungespace(),coord(),integer()) -> true.
set_atomic(Fungespace, {_X,_Y} = Coord, Value) ->
	gen_server:call(?CALL_NAME, {set_atomic, Fungespace, Coord, Value}).

-spec fetch_atomic(fungespace(),coord()) -> integer().
fetch_atomic(Fungespace, {_X,_Y} = Coord) ->
	gen_server:call(?CALL_NAME, {fetch_atomic, Fungespace, Coord}).

-spec cmpxchg(fungespace(), coord(), integer(), integer()) -> cmpxchg_return().
cmpxchg(Fungespace, {_X,_Y} = Coord, OldValue, NewValue) ->
	gen_server:call(?CALL_NAME, {cmpxchg, Fungespace, Coord, OldValue, NewValue}).


%%====================================================================
%% API - non-calls
%%====================================================================

%% @spec set(fungespace(), ip(), coord(), V::integer()) -> true
%% @doc Set a cell in Funge Space with storage offset taken from IP.
-spec set(fungespace(), ip(), coord(), integer()) -> true.
set(Fungespace, #fip{offX = OffX, offY = OffY}, {X,Y}, V) ->
	set(Fungespace, {X+OffX, Y+OffY}, V).

%% @spec set(fungespace(), coord(), V::integer()) -> true
%% @doc Set a cell in Funge Space.
-spec set(fungespace(), coord(), integer()) -> true.
set(Fungespace, {_X,_Y} = Coord, $\s) ->
	ets:delete(Fungespace, Coord),
	cast_update_bounds($\s, Fungespace, Coord);
set(Fungespace, {_X,_Y} = Coord, V) ->
	ets:insert(Fungespace, {Coord, V}),
	cast_update_bounds(V, Fungespace, Coord).

%% @spec fetch(fungespace(), ip(), coord()) -> integer()
%% @doc Get a cell from a specific Funge Space. Will use storage offset of IP.
-spec fetch(fungespace(), ip(), coord()) -> cell().
fetch(Fungespace, #fip{offX = OffX, offY = OffY}, {X,Y}) ->
	fetch(Fungespace, {X+OffX, Y+OffY}).

%% @spec fetch(fungespace(), coord()) -> integer()
%% @doc Get a cell from a specific Funge Space.
-spec fetch(fungespace(), coord()) -> cell().
fetch(Fungespace, {_X,_Y} = Coord) ->
	case ets:lookup(Fungespace, Coord) of
		[] -> $\s;
		[{_,Value}] -> Value
	end.

%% @spec load(fungespace(), ip(), Filename::string(), IsBinaryMode::bool(), coord()) -> error | coord()
%% @doc Loads a file into an existing Funge Space, returning the max size.
-spec load(fungespace(), ip(), string(), bool(), coord()) -> error | coord().
load(Fungespace, #fip{offX = OffX, offY = OffY}, Filename, IsBinaryMode, {X, Y} = _Coord) ->
	{Status, Binary} = file:read_file(Filename),
	% TODO: Make binary mode work.
	case Status of
		error ->
			error;
		ok ->
			TrueX = OffX + X,
			TrueY = OffY + Y,
			case IsBinaryMode of
				false ->
					{MaxX, MaxY} = load_binary(Binary, Fungespace, TrueX, TrueY, false, TrueX, undefined);
				true ->
					MaxX = load_binary_no_newlines(Binary, Fungespace, TrueX, TrueY),
					MaxY = TrueY
			end,
			{MaxX - TrueX, MaxY - TrueY}
	end.

%% @spec get_bounds(fungespace()) -> {LeastPoint::coord(), GreatestPoint::coord()}
%% @doc Get Funge Space bounds from global data only.
-spec get_bounds(fungespace()) -> {coord(), coord()}.
get_bounds(Fungespace) ->
	[{_,Min,Max}] = ets:lookup(Fungespace, bounds),
	{Min, Max}.

%% @spec get_bounds(fungespace()) -> {LeastPoint::coord(), GreatestPoint::coord()}
%% @doc Get Funge Space bounds from the thread local bounds.
-spec get_bounds_thread() -> {coord(), coord()}.
get_bounds_thread() ->
	get(efunge_bounds).

%% @spec get_bounds_thread_exact(fungespace()) -> {LeastPoint::coord(), GreatestPoint::coord()}
%% @doc Get exact Funge Space bounds, or thread local ones.
-spec get_bounds_thread_exact(fungespace()) -> {coord(), coord()}.
get_bounds_thread_exact(Fungespace) ->
	case get(efunge_bounds_exact) of
		true ->
			get(efunge_bounds);
		false ->
			recalculate_bounds_exact(Fungespace)
	end.

%% @spec get_block(fungespace(), Min::coord(), Max::coord()) -> list({coord(), Value})
%% @doc Returns a block of funge space, only return those values actually set in
%% said block, non-present ones are space. This is absolute, not relative offset.
%%
%% Note this is experimental and may or may not be removed in the future.
-spec get_block(fungespace(),coord(),coord()) -> [{coord(), integer()}].
get_block(Fungespace, {MinX, MinY}, {MaxX, MaxY}) ->
	Results = ets:select(Fungespace,
	                     [{{{'$1','$2'}, '$3'},
	                       [{'>=', '$1', MinX},
	                        {'=<', '$1', MaxX},
	                        {'>=', '$2', MinY},
	                        {'=<', '$2', MaxY}],
	                       ['$_']}
	                     ]),
	Results.


%%====================================================================
%% API - Utility functions
%%====================================================================

%% @doc Is X, Y in range of the box created by the second parameter?
-spec is_in_range(coord(),{coord(),coord()}) -> bool().
is_in_range({X, Y}, {{MinX, MinY}, {MaxX, MaxY}}) ->
	if
		X < MinX -> false;
		X > MaxX -> false;
		true     ->
			if
				Y < MinY -> false;
				Y > MaxY -> false;
				true     -> true
			end
	end.

%% @doc Set bounds in process dict. Use once at start of thread.
-spec set_process_bounds_initial(fungespace()) -> ok.
set_process_bounds_initial(Fungespace) ->
	put(efunge_bounds, get_bounds(Fungespace)),
	%% TODO: Work out if they are exact or not instead.
	put(efunge_bounds_exact, false),
	ok.


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%% @hidden
%% @doc Initiates the server
-spec init([]) -> {ok, state()}.
init([]) ->
	process_flag(trap_exit, true),
	{ok, construct()}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} | {stop, Reason, Reply, State}
%% @hidden
%% @doc Handling call messages
-spec handle_call(call_arg(), {pid(), _}, state()) -> call_return().
handle_call(get_fungespace, _From, State) ->
	{reply, State, State};

handle_call({load_initial, Fungespace, Filename}, _From, State) ->
	Reply = load_at_origin(Fungespace, Filename),
	{reply, Reply, State};

handle_call({set_atomic, Fungespace, Coord, Value}, _From, State) ->
	Reply = set(Fungespace, Coord, Value),
	{reply, Reply, State};

handle_call({fetch_atomic, Fungespace, Coord}, _From, State) ->
	Reply = fetch(Fungespace, Coord),
	{reply, Reply, State};

handle_call({cmpxchg, Fungespace, Coord, OldValue, NewValue}, _From, State) ->
	case fetch(Fungespace, Coord) of
		OldValue  ->
			set(Fungespace, Coord, NewValue),
			Reply = {ok, OldValue};
		RealValue ->
			Reply = {failed, RealValue}
	end,
	{reply, Reply, State};

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State}.

%% @spec handle_cast(Msg, State) -> {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State}
%% @hidden
%% @doc Handling cast messages
-spec handle_cast(update_bounds,state()) -> {noreply,state()}.
handle_cast({update_bounds, Fungespace, Coord}, State) ->
	update_bounds(Fungespace, Coord),
	{noreply, State};
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
terminate(_Reason, State) ->
	delete(State),
	ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @hidden
%% @doc Convert process state when code is changed
-spec code_change(_,state(),_) -> {'ok',state()}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%====================================================================
%% Internal functions - utility functions
%%====================================================================

%% Extend rect to contain point.
-spec rect_point_union(coord(),rect()) -> rect().
rect_point_union({X, Y}, {{MinX,MinY},{MaxX,MaxY}}) ->
	MinX1 = find_bounds_min(MinX, X),
	MinY1 = find_bounds_min(MinY, Y),
	MaxX1 = find_bounds_max(MaxX, X),
	MaxY1 = find_bounds_max(MaxY, Y),
	{{MinX1, MinY1}, {MaxX1, MaxY1}}.

%% Largest rect of the two.
-spec rect_union(rect(), rect()) -> rect().
rect_union({{MinAX,MinAY},{MaxAX,MaxAY}}, {{MinBX,MinBY},{MaxBX,MaxBY}}) ->
	MinX1 = find_bounds_min(MinAX, MinBX),
	MinY1 = find_bounds_min(MinAY, MinBY),
	MaxX1 = find_bounds_max(MaxAX, MaxBX),
	MaxY1 = find_bounds_max(MaxAY, MaxBY),
	{{MinX1, MinY1}, {MaxX1, MaxY1}}.

%% @doc Finds minimum.
-spec find_bounds_min(integer_or_undef(), integer()) -> integer().
find_bounds_min(undefined, Y)    -> Y;
find_bounds_min(X, Y) when X < Y -> X;
find_bounds_min(_X, Y)           -> Y.

%% @doc Finds maximum.
-spec find_bounds_max(integer_or_undef(), integer()) -> integer().
find_bounds_max(undefined, Y)    -> Y;
find_bounds_max(X, Y) when X > Y -> X;
find_bounds_max(_X, Y)           -> Y.

%% @doc Find the extreme values in the list.
-spec find_extremes([{cell(),cell()}],cell(),cell(),cell(),cell()) -> {coord(), coord()}.
find_extremes([], MinX, MinY, MaxX, MaxY) ->
	{{MinX,MinY},{MaxX,MaxY}};
find_extremes([{X,Y}|T], MinX, MinY, MaxX, MaxY) ->
	find_extremes(T, erlang:min(MinX,X),erlang:min(MinY,Y),
	                 erlang:max(MaxX,X),erlang:max(MaxY,Y)).


%%====================================================================
%% Internal functions - client
%%====================================================================

-spec cast_update_bounds(integer(),fungespace(),coord()) -> true.
cast_update_bounds($\s, _Space, {X,Y}) ->
	case get(efunge_bounds_exact) of
		false -> true;
		true  ->
			{{MinX,MinY},{MaxX,MaxY}} = get(efunge_bounds),
			if
				X =:= MinX; X =:= MaxX; Y =:= MinY; Y =:= MaxY ->
					put(efunge_bounds_exact, false),
					true;
				true ->
					true
			end
	end;
cast_update_bounds(_V, Space, Coord) ->
	Bounds = get(efunge_bounds),
	case is_in_range(Coord, Bounds) of
		false ->
			gen_server:cast(?CALL_NAME, {update_bounds, Space, Coord}),
			OldThreadBounds = get(efunge_bounds),
			NewBounds = rect_union(OldThreadBounds, rect_point_union(Coord, Bounds)),
			put(efunge_bounds, NewBounds),
			true;
		true ->
			true
	end.

-spec recalculate_bounds_exact(fungespace()) -> {coord(), coord()}.
recalculate_bounds_exact(Fungespace) ->
	% Get first item as base for new bounds.
	[{FirstX,FirstY}|Coordinates] = ets:select(Fungespace, [{{'$1','$2'},[{'=/=','$2',$\s}],['$1']}]),
	NewBounds = find_extremes(Coordinates, FirstX, FirstY, FirstX, FirstY),
	put(efunge_bounds, NewBounds),
	put(efunge_bounds_exact, true),
	NewBounds.


%%====================================================================
%% Internal functions - server
%%====================================================================

%% @doc Construct a Funge Space.
-spec construct() -> fungespace().
construct() ->
	Space = ets:new(fungespace, [set, public]),
	ets:insert(Space, {bounds, {undefined, undefined}, {undefined, undefined}}),
% TODO
% 	put(fspacebounds_exact, true),
	Space.

%% @spec delete(fungespace()) -> true
%% @doc Destroy a Funge Space.
-spec delete(fungespace()) -> true.
delete(Fungespace) ->
	ets:delete(Fungespace).


%% @spec load_at_origin(Filename::string()) -> fungespace()
%% @doc Load a Funge Space at 0,0.
-spec load_at_origin(fungespace(), string()) -> notfound | ok.
load_at_origin(Fungespace, Filename) ->
	{Result, Binary} = file:read_file(Filename),
	case Result of
		error -> notfound;
		ok    -> load_initial(Binary, Fungespace, 0, 0, false)
	end.

%% @spec set_server(fungespace(), coord(), V::integer()) -> true
%% @doc Set a cell in Funge Space.
-spec set_server(fungespace(), coord(), integer()) -> true.
set_server(Fungespace, {_X,_Y} = Coord, V) ->
	ets:insert(Fungespace, {Coord, V}),
	update_bounds(V, Fungespace, Coord).


%%====================================================================
%% Internal functions - Client/Server - Bounds updates.
%%====================================================================

%% @doc Update bounds values in tables, do not care about value.
-spec update_bounds(fungespace(), coord()) -> true.
update_bounds(Space, {X,Y}) ->
	[{_,{MinX,MinY},{MaxX,MaxY}}] = ets:lookup(Space, bounds),
	MinX1 = find_bounds_min(MinX, X),
	MinY1 = find_bounds_min(MinY, Y),
	MaxX1 = find_bounds_max(MaxX, X),
	MaxY1 = find_bounds_max(MaxY, Y),
	ets:insert(Space, {bounds, {MinX1, MinY1}, {MaxX1, MaxY1}}),
	true.

%% @doc Update bounds values in tables, only if value isn't space.
-spec update_bounds(integer(), fungespace(), coord()) -> true.
update_bounds($\s, _Space, _Coord) ->
	true;
update_bounds(_V, Space, {X,Y}) ->
	[{_,{MinX,MinY},{MaxX,MaxY}}] = ets:lookup(Space, bounds),
	MinX1 = find_bounds_min(MinX, X),
	MinY1 = find_bounds_min(MinY, Y),
	MaxX1 = find_bounds_max(MaxX, X),
	MaxY1 = find_bounds_max(MaxY, Y),
	ets:insert(Space, {bounds, {MinX1, MinY1}, {MaxX1, MaxY1}}),
	true.


%%====================================================================
%% Internal functions - Server - Bulk loading.
%%====================================================================


%% @spec load_initial(Binary, fungespace(), X, Y, LastWasCR, MinX, MaxX) -> coord()
%% @doc
%% Load a binary into Funge Space. Loads at 0,0. Assumes no race conditions.
-spec load_initial(binary(),fungespace(),integer(),integer(),bool()) -> ok.
load_initial(<<H,T/binary>>, FungeSpace, X, Y, LastWasCR) ->
	case H of
		$\n ->
			case LastWasCR of
				true -> load_initial(T, FungeSpace, 0, Y, false);
				false -> load_initial(T, FungeSpace, 0, Y+1, false)
			end;
		$\r ->
			load_initial(T, FungeSpace, 0, Y+1, true);
		%% Spaces shouldn't replace.
		$\s ->
			load_initial(T, FungeSpace, X+1, Y, false);
		_ ->
			set_server(FungeSpace, {X, Y}, H),
			load_initial(T, FungeSpace, X+1, Y, false)
	end;
load_initial(<<>>, _FungeSpace, _X, _Y, _LastWasCR) ->
	ok.


%% @spec load_binary(Binary, fungespace(), X, Y, LastWasCR, MinX, MaxX) -> coord()
%% @doc
%% Load a binary into Funge Space. MinX is used for knowing what least X
%% should be used when resetting due to newline, and not loading from 0,0
%% MaxX is used for making return value work.
-spec load_binary(binary(),fungespace(),integer(),integer(),bool(),integer(),integer_or_undef()) -> coord().
load_binary(<<H,T/binary>>, FungeSpace, X, Y, LastWasCR, MinX, MaxX) ->
	case H of
		$\n ->
			case LastWasCR of
				true -> load_binary(T, FungeSpace, MinX, Y, false, MinX, MaxX);
				false -> load_binary(T, FungeSpace, MinX, Y+1, false, MinX, find_bounds_max(MaxX, X))
			end;
		%% Form feed is ignored.
		$\f ->
			load_binary(T, FungeSpace, X, Y, false, MinX, MaxX);
		$\r ->
			load_binary(T, FungeSpace, MinX, Y+1, true, MinX, find_bounds_max(MaxX, X));
		%% Spaces shouldn't replace.
		$\s ->
			load_binary(T, FungeSpace, X+1, Y, false, MinX, MaxX);
		_ ->
			set(FungeSpace, {X, Y}, H),
			load_binary(T, FungeSpace, X+1, Y, false, MinX, MaxX)
	end;
load_binary(<<>>, _FungeSpace, X, Y, _LastWasCR, _MinX, MaxX) ->
	{find_bounds_max(MaxX, X), Y}.

%% @spec load_binary_no_newlines(Binary, fungespace(), X, Y) -> MaxX
%% @doc Load everything in the binary, without going to a new row on newline.
-spec load_binary_no_newlines(binary(),fungespace(),integer(),integer()) -> integer().
load_binary_no_newlines(<<H,T/binary>>, FungeSpace, X, Y) ->
	set(FungeSpace, {X, Y}, H),
	load_binary_no_newlines(T, FungeSpace, X+1, Y);
load_binary_no_newlines(<<>>, _FungeSpace, X, _Y) ->
	X.
