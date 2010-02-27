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
%% @doc TURT fingerprint.
-module(fingTURT).
-export([load/1]).
%% The implemented functions
-export([turt_query_heading/3,
         turt_back/3,
         turt_pen_colour/3,
         turt_show_display/3,
         turt_query_pen/3,
         turt_forward/3,
         turt_set_heading/3,
         turt_print_drawing/3,
         turt_turn_left/3,
         turt_clear_paper/3,
         turt_pen_position/3,
         turt_query_position/3,
         turt_turn_right/3,
         turt_teleport/3,
         turt_query_bounds/3]).

-include("../efunge_ip.hrl").
-include("../funge_types.hrl").

%% Import common functions:
-import(efunge_stackstack, [push/2, pop/1]).


%% @doc Load the TURT fingerprint.
-spec load(ip()) -> {ok, ip()} | {error, ip()}.
load(IP) ->
	case start_turtle() of
		ok ->
			IP2 = efunge_fingermanager:push_funs(IP, [
				{$A, fun ?MODULE:turt_query_heading/3},
				{$B, fun ?MODULE:turt_back/3},
				{$C, fun ?MODULE:turt_pen_colour/3},
				{$D, fun ?MODULE:turt_show_display/3},
				{$E, fun ?MODULE:turt_query_pen/3},
				{$F, fun ?MODULE:turt_forward/3},
				{$H, fun ?MODULE:turt_set_heading/3},
				{$I, fun ?MODULE:turt_print_drawing/3},
				{$L, fun ?MODULE:turt_turn_left/3},
				{$N, fun ?MODULE:turt_clear_paper/3},
				{$P, fun ?MODULE:turt_pen_position/3},
				{$Q, fun ?MODULE:turt_query_position/3},
				{$R, fun ?MODULE:turt_turn_right/3},
				{$T, fun ?MODULE:turt_teleport/3},
				{$U, fun ?MODULE:turt_query_bounds/3}]),
			{ok, IP2};
		{error,_} -> {error,IP}
	end.


%% The fingerprint functions

%% @spec turt_query_heading(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc A - Query Position (x, y coordinates)
-spec turt_query_heading(ip(), stackstack(), fungespace()) -> return_normal().
turt_query_heading(IP, Stack, _Space) ->
	{_Pos, Heading, _PenDown, _Bounds} = efunge_turtle:get_info(),
	{IP, push(Stack, Heading)}.

%% @spec turt_back(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc B - Back (distance in pixles)
-spec turt_back(ip(), stackstack(), fungespace()) -> return_normal().
turt_back(IP, Stack, _Space) ->
	{S1, Distance} = pop(Stack),
	ok = efunge_turtle:move(-Distance),
	{IP, S1}.

%% @spec turt_pen_colour(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc C - Pen Colour (24-bit RGB)
-spec turt_pen_colour(ip(), stackstack(), fungespace()) -> return_normal().
turt_pen_colour(IP, Stack, _Space) ->
	{S1, Colour} = pop(Stack),
	ok = efunge_turtle:set_pen_colour(integer_to_colour(Colour)),
	{IP, S1}.

%% @spec turt_show_display(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc D - Show Display (0 = no, 1 = yes)
-spec turt_show_display(ip(), stackstack(), fungespace()) -> return_normal().
turt_show_display(IP, Stack, _Space) ->
	{S1, Value} = pop(Stack),
	%% Since we don't implement any display, just ignore the valid values, and
	%% reflect on any other.
	if
		Value =:= 0; Value =:= 1 -> {IP, S1};
		true -> {efunge_ip:rev_delta(IP), S1}
	end.

%% @spec turt_query_pen(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc E - Query Pen (0 = up, 1 = down)
-spec turt_query_pen(ip(), stackstack(), fungespace()) -> return_normal().
turt_query_pen(IP, Stack, _Space) ->
	{_Pos, _Heading, PenDown, _Bounds} = efunge_turtle:get_info(),
	case PenDown of
		true ->	{IP, push(Stack, 1)};
		false -> {IP, push(Stack, 0)}
	end.

%% @spec turt_forward(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc F - Forward (distance in pixels)
-spec turt_forward(ip(), stackstack(), fungespace()) -> return_normal().
turt_forward(IP, Stack, _Space) ->
	{S1, Distance} = pop(Stack),
	ok = efunge_turtle:move(Distance),
	{IP, S1}.

%% @spec turt_set_heading(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc H - Set Heading (angle in degrees, relative to 0deg, east)
-spec turt_set_heading(ip(), stackstack(), fungespace()) -> return_normal().
turt_set_heading(IP, Stack, _Space) ->
	{S1, Angle} = pop(Stack),
	ok = efunge_turtle:set_heading(Angle),
	{IP, S1}.

%% @spec turt_print_drawing(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc I - Print current Drawing (if possible)
-spec turt_print_drawing(ip(), stackstack(), fungespace()) -> return_normal().
turt_print_drawing(IP, Stack, _Space) ->
	{svg,Image} = efunge_turtle:render(svg),
	case file:write_file("efunge_TURT.svg", Image) of
		ok -> {IP, Stack};
		{error,_} -> {efunge_ip:rev_delta(IP), Stack}
	end.

%% @spec turt_turn_left(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc L - Turn Left (angle in degrees)
-spec turt_turn_left(ip(), stackstack(), fungespace()) -> return_normal().
turt_turn_left(IP, Stack, _Space) ->
	{S1, Angle} = pop(Stack),
	ok = efunge_turtle:rotate(-Angle),
	{IP, S1}.

%% @spec turt_clear_paper(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc N - Clear Paper with Colour (24-bit RGB)
-spec turt_clear_paper(ip(), stackstack(), fungespace()) -> return_normal().
turt_clear_paper(IP, Stack, _Space) ->
	{S1, Colour} = pop(Stack),
	ok = efunge_turtle:clear(),
	ok = efunge_turtle:set_bg_colour(integer_to_colour(Colour)),
	{IP, S1}.


%% @spec turt_pen_position(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc P - Pen Position (0 = up, 1 = down)
-spec turt_pen_position(ip(), stackstack(), fungespace()) -> return_normal().
turt_pen_position(IP, Stack, _Space) ->
	{S1, Position} = pop(Stack),
	P = case Position of
		0 -> false;
		1 -> true
	end,
	ok = efunge_turtle:set_pen_state(P),
	{IP, S1}.

%% @spec turt_query_position(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc Q - Query Position (x, y coordinates)
-spec turt_query_position(ip(), stackstack(), fungespace()) -> return_normal().
turt_query_position(IP, Stack, _Space) ->
	{{X,Y}, _Heading, _PenDown, _Bounds} = efunge_turtle:get_info(),
	S1 = push(Stack, X),
	{IP, push(S1, Y)}.

%% @spec turt_turn_right(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc R - Turn Right (angle in degrees)
-spec turt_turn_right(ip(), stackstack(), fungespace()) -> return_normal().
turt_turn_right(IP, Stack, _Space) ->
	{S1, Angle} = pop(Stack),
	ok = efunge_turtle:rotate(Angle),
	{IP, S1}.

%% @spec turt_teleport(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc T - Teleport (x, y coords relative to origin; 00T = home)
-spec turt_teleport(ip(), stackstack(), fungespace()) -> return_normal().
turt_teleport(IP, Stack, _Space) ->
	{S1, Y} = pop(Stack),
	{S2, X} = pop(S1),
	ok = efunge_turtle:jump({X,Y}),
	{IP, S2}.

%% @spec turt_query_bounds(ip(), stackstack(), fungespace()) -> return_normal()
%% @doc U - Query Bounds (two pairs of x, y coordinates)
-spec turt_query_bounds(ip(), stackstack(), fungespace()) -> return_normal().
turt_query_bounds(IP, Stack, _Space) ->
	{_Pos, _Heading, _PenDown, Bounds} = efunge_turtle:get_info(),
	{{X1,Y1},{X2,Y2}} =
		case Bounds of
			none -> {{0,0},{0,0}};
			_ -> Bounds
		end,
	S1 = push(Stack, X1),
	S2 = push(S1, Y1),
	S3 = push(S2, X2),
	S4 = push(S3, Y2),
	{IP, S4}.

%% Private funtions

%% Start the turtle process if required.
-spec start_turtle() -> 'ok' | {'error',{'supervisor',_}}.
start_turtle() ->
	ChildSpec = {'efunge_turtle', {'efunge_turtle', start_link, []},
	             permanent, 2000, worker, [efunge_turtle]},
	case efunge_supervisor_services:add_service(ChildSpec) of
		%% Normal ones:
		{ok, _Child} -> ok;
		{ok, _Child, _Info} -> ok;
		{error,{already_started,_Child}} -> ok;
		%% Abnormal ones (TODO: Log warning?)
		{already_present,{ok,_Child}} -> ok;
		{already_present,{ok,_Child,_Info}} -> ok;
		%% Fatal ones:
		{already_present,Error} -> {error,{supervisor,[already_present,Error]}};
		Result -> {error,{supervisor,Result}}
	end.

-spec integer_to_colour(integer()) -> efunge_turtle:colour().
integer_to_colour(Value) ->
	<<R,G,B>> = <<Value:24/integer>>,
	{R,G,B}.
