%%%----------------------------------------------------------------------
%%% efunge - a Befunge-98 interpreter in Erlang.
%%% Copyright (C) 2008-2008 Arvid Norlander <anmaster AT tele2 DOT se>
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
%% @doc Error logger module.
-module(efunge_report).

-behaviour(gen_event).
%% API
-export([register_handler/0, register_handler/2, unregister_handler/0]).
-export([error/1, warning/1, info/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-record(state,
	{
		enable_warnings = false :: bool(),
		enable_info     = false :: bool()
	}).

-type state() :: #state{}.

-type report() :: [{_,_}|_]|any().

-type error_report()   :: {'error_report'  ,pid(),{pid(),'efunge_error'  ,report()}}.
-type warning_report() :: {'warning_report',pid(),{pid(),'efunge_warning',report()}}.
-type info_report()    :: {'info_report'   ,pid(),{pid(),'efunge_info'   ,report()}}.
-type event() :: error_report() | warning_report() | info_report().


%%====================================================================
%% API
%%====================================================================

%% @doc Register this handler.
-spec register_handler() -> any().
register_handler() ->
	error_logger:add_report_handler(?MODULE, {false, false}).

%% @doc Register this handler. Special arguments.
-spec register_handler(bool(),bool()) -> any().
register_handler(Warnings, Info) ->
	error_logger:add_report_handler(?MODULE, {Warnings, Info}).

%% @doc Unregister this handler.
-spec unregister_handler() -> any().
unregister_handler() ->
	error_logger:delete_report_handler(?MODULE).

%% @doc Report error.
-spec error(report()) -> 'ok'.
error(Report) ->
	error_logger:warning_report(efunge_error, Report).

%% @doc Report warning.
-spec warning(report()) -> 'ok'.
warning(Report) ->
	error_logger:warning_report(efunge_warning, Report).

%% @doc Report info.
-spec info(report()) -> 'ok'.
info(Report) ->
	error_logger:warning_report(efunge_info, Report).


%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
-spec init({bool(),bool()}) -> {ok,#state{}}.
init({Warnings, Info}) ->
	{ok, #state{ enable_warnings = Warnings, enable_info = Info}}.

%%--------------------------------------------------------------------
%% Function:
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event.
%%--------------------------------------------------------------------
%% Handle those we care for. Ignore otherwise.
-spec handle_event(event(), state()) -> {'ok',state()}.
handle_event({error_report, _Gleader, {Pid, efunge_error, Report}}, State) ->
	io:put_chars([generate_header("ERROR"),
	              generate_standard_entry("Pid", Pid),
	              generate_report(Report, [])]),
	{ok, State};
handle_event({warning_report, _Gleader, {Pid, efunge_warning, Report}}, #state{ enable_warnings = true } = State) ->
	io:put_chars([generate_header("WARNING"),
	              generate_standard_entry("Pid", Pid),
	              generate_report(Report, [])]),
	{ok, State};
handle_event({info_report, _Gleader, {Pid, efunge_info, Report}}, #state{ enable_info = true } = State) ->
	io:put_chars([generate_header("INFO"),
	              generate_standard_entry("Pid", Pid),
	              generate_report(Report, [])]),
	{ok, State};
handle_event(_Event, State) ->
	{ok, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1,
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% Description: Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event
%% handler to handle the request.
%%--------------------------------------------------------------------
-spec handle_call(_,state()) -> {ok,unkown_call,state()}.
handle_call(Request, State) ->
	io:format("~p: Unknown call ~p!~n", [?MODULE, Request]),
	{ok, unkown_call, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% Description: This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%%--------------------------------------------------------------------
-spec handle_info(_,state()) -> {ok,state()}.
handle_info(_Info, State) ->
	{ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description:Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%--------------------------------------------------------------------
-spec terminate(_,state()) -> ok.
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
-spec code_change(_,state(),_) -> {ok,state()}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

%% This code is a black-box reimplementation of the Erlang error reporter
%% format output. This due to license issues. Also due to only testing the
%% parts I needed it may or may not be complete.


%% Used to generate the normal report. Only accepts standard format.
%% In case of anything else it will just do ~p~n format for that data.
-spec generate_report(report(), iolist()) -> [[any()] | char()].
generate_report([], Report) ->
	Report;
generate_report([{Name,Data}|Tail], Report) ->
	S = io_lib:format("    ~p: ~p~n", [Name, Data]),
	generate_report(Tail, [S|Report]);
generate_report([OtherData|Tail], Report) ->
	S = io_lib:format("    ~p~n", [OtherData]),
	generate_report(Tail, [S|Report]);
generate_report(Other, _Report) ->
	io_lib:format("~p~n", Other).


%% Used to add caller PID to all reports and such.
-spec generate_standard_entry(string()|binary()|iolist(),_) -> [[any()] | char()].
generate_standard_entry(Name, Data) ->
	io_lib:format("    ~s: ~p~n", [Name, Data]).

%% @doc Generates the header
-spec generate_header(iolist()) -> [[any()] | char()].
generate_header(Type) ->
	io_lib:format("=EFUNGE ~s REPORT=== ~s ===~n", [Type, generate_time()]).

%% @doc Generates the time part of the header
-spec generate_time() -> [[any()] | char()].
generate_time() ->
	{{Y, Month, D}, {H, M, S}} = erlang:localtime(),
	io_lib:format("~2..0B-~s-~p::~2..0B:~2..0B:~2..0B", [D, month_name(Month), Y, H, M, S]).

%% @doc Map month number to string.
-spec month_name(1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12) -> string().
month_name(1)  -> "Jan";
month_name(2)  -> "Feb";
month_name(3)  -> "Mar";
month_name(4)  -> "Apr";
month_name(5)  -> "May";
month_name(6)  -> "Jun";
month_name(7)  -> "Jul";
month_name(8)  -> "Aug";
month_name(9)  -> "Sep";
month_name(10) -> "Oct";
month_name(11) -> "Nov";
month_name(12) -> "Dec".
