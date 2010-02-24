%%%----------------------------------------------------------------------
%%% efunge - a Befunge-98 interpreter in Erlang.
%%% Copyright (C) 2010 Arvid Norlander <anmaster AT tele2 DOT se>
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
%% @doc
%% A gen_server turtle bot.
%%
%% Note: coodinate system is +x at right, +y at bottom.
%% Headings are clockwise with 0 at +x axis.
%% Thus 90 degrees is straight down and 180 along -x axis.
%%
-module(efunge_turtle).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, stop/0]).

-export([get_info/0, clear/0, render/1]).
-export([rotate/1, set_heading/1]).
-export([jump/1, move/1]).
-export([set_bg_colour/1, set_pen_colour/1, set_pen_state/1]).


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
%% Types and records
%%====================================================================

-type colour()  :: {Red::byte(), Green::byte(), Blue::byte()}.
-type tcoord()  :: {X::integer(), Y::integer()}.
-type tbounds() :: none | {TopLeft::tcoord(), BottomRight::tcoord()}.
-type angle_deg() :: integer().

-type circle() :: {circle,colour(),tcoord()}.
-type line()   :: {line,colour(),[tcoord()]}.
-type tnode()  :: circle() | line().

-type image_formats() :: svg | raw.

-record(turtle,
	{
		pos     = {0,0}   :: tcoord(),
		heading = 0       :: angle_deg(),
		sin     = 0.0     :: float(),
		cos     = 1.0     :: float(),
		%% If true pen is down.
		pen     = false   :: boolean(),
		colour  = {0,0,0} :: colour()
	}).

-record(drawing,
	{
		nodes    = []            :: [tnode()],
		bounds   = none          :: tbounds(),
		bgcolour = none          :: none | colour()
	}).

-type turtle() :: #turtle{}.
-type drawing() :: #drawing{}.
-type state() :: {turtle(),drawing()}.

-type turtle_info() :: {Position::tcoord(), Heading::integer(),
                        PenDown::boolean(), Bounds::tbounds()}.

-include("../../otp_types.hrl").

%% FIXME!
-type call_return_replies() :: ok | turtle_info().
-type call_return_reply()   :: {reply, call_return_replies(), state()}.
-type call_return_stop()    :: {stop,normal,stopped,state()}.
-type call_return()         :: call_return_reply() | call_return_stop().
%% Grouped by type of parameters.
-type call_op_none()        :: get_info | clear | stop.
-type call_op_colour()      :: set_bg_colour | set_pen_colour.
-type call_op_heading()     :: set_heading | rotate.
-type call_op_misc()        :: {set_pen_state, Down::boolean()}
                             | {jump, tcoord()}
                             | {render, image_formats()}.
-type call_op()             :: call_op_none()
                             | {call_op_colour(), colour()}
                             | {call_op_heading(), Degrees::angle_deg()}
                             | call_op_misc().


%%====================================================================
%% API - Generic start/stop stuff
%%====================================================================

%% @spec start_link() -> {ok,Pid} | {error,Error}
%% @doc Starts the server, linked to supervisor.
-spec start_link() -> otp_start_return_no_ignore().
start_link() ->
	gen_server:start_link(?REGISTER_NAME, ?MODULE, [], []).

%% @spec start() -> {ok,Pid} | {error,Error}
%% @doc Starts the server, standalone.
-spec start() -> otp_start_return_no_ignore().
start() ->
	gen_server:start(?REGISTER_NAME, ?MODULE, [], []).

-spec stop() -> stopped.
stop() ->
	gen_server:call(?CALL_NAME, stop).


%%====================================================================
%% API - Calls
%%====================================================================

%% @doc Returns info about current position and so on.
-spec get_info() -> turtle_info().
get_info() ->
	gen_server:call(?CALL_NAME, get_info).

%% @doc
%% Resets the drawing to default. The turtle bot's position, heading, pen state
%% and pen colour are kept.
%% TODO: Fix dot after clear() with pen down!
-spec clear() -> ok.
clear() ->
	gen_server:call(?CALL_NAME, clear).

%% @doc Rotates the bot a number of degrees (positive = clockwise)
-spec rotate(angle_deg()) -> ok.
rotate(Angle) when is_integer(Angle) ->
	gen_server:call(?CALL_NAME, {rotate,Angle}).

-spec jump(tcoord()) -> ok.
jump({X,Y}=Position) when is_integer(X), is_integer(Y) ->
	gen_server:call(?CALL_NAME, {jump,Position}).

-spec move(integer()) -> ok.
move(Distance) when is_integer(Distance) ->
	gen_server:call(?CALL_NAME, {move,Distance}).

%% @doc Sets heading in degrees, 0 is east, Goes clockwise.
-spec set_heading(angle_deg()) -> ok.
set_heading(Angle) when is_integer(Angle) ->
	gen_server:call(?CALL_NAME, {set_heading,Angle}).

%% Throws exception {turt_invalid_colour,InvalidColour} on non-valid parameter.
-spec set_bg_colour(colour()) -> ok.
set_bg_colour(Colour = {_,_,_}) ->
	validate_colour(Colour),
	gen_server:call(?CALL_NAME, {set_bg_colour,Colour}).

%% Throws exception {turt_invalid_colour,InvalidColour} on non-valid parameter.
-spec set_pen_colour(colour()) -> ok.
set_pen_colour(Colour = {_,_,_}) ->
	validate_colour(Colour),
	gen_server:call(?CALL_NAME, {set_pen_colour,Colour}).

-spec set_pen_state(boolean()) -> ok.
set_pen_state(Down) when is_boolean(Down) ->
	gen_server:call(?CALL_NAME, {set_pen_state,Down}).

%% @doc
%% Renders image to a given image format.
%% Currently supported formats: svg, raw.
%% Raw is just a dump for debugging purposes.
-spec render(image_formats()) -> {svg,iolist()}|{raw,[tnode()]}.
render(svg) ->
	gen_server:call(?CALL_NAME, {render,svg});
render(raw) ->
	gen_server:call(?CALL_NAME, {render,raw}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%% @hidden
%% @doc Initiates the server
-spec init([]) -> {ok, state()}.
init([]) ->
	{ok, {#turtle{},#drawing{}}}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} | {stop, Reason, Reply, State}
%% @hidden
%% @doc Handling call messages
-spec handle_call(call_op(),_,state()) -> call_return().
handle_call(get_info, _From, {T,D} = State) ->
	{reply, {T#turtle.pos, T#turtle.heading, T#turtle.pen, D#drawing.bounds}, State};

%% Clear. Add a circle straight away if pen is down.
handle_call(clear, _From, {T=#turtle{pen=false},_D}) ->
	{reply, ok, {T, #drawing{}}};
handle_call(clear, _From, {T=#turtle{pen=true},_D}) ->
	{reply, ok, {T, add_circle(T,#drawing{})}};

handle_call({render,Format}, _From, {T,D}) ->
	ND = prune_circles(D),
	Image = render(Format, ND),
	{reply, Image, {T, ND}};

%% Rotate/heading
handle_call({rotate,Angle}, _From, {T,D}) ->
	{reply, ok, {rotate(T,Angle), D}};

handle_call({set_heading,Angle}, _From, {T,D}) ->
	{reply, ok, {set_heading(T,Angle), D}};

%% Move and jump
handle_call({jump,Position}, _From, {T,D}) ->
	NT = T#turtle{pos=Position},
	{reply, ok, {NT,add_circle(NT,D)}};

handle_call({move,Distance}, _From, {T,D}) ->
	{reply, ok, move(T,D,Distance)};

%% Pen state
handle_call({set_pen_state, false}, _From, {T,D}) ->
	{reply, ok, {T#turtle{pen=false}, D}};
handle_call({set_pen_state, true}, _From, {#turtle{pen=true},_D}=State) ->
	{reply, ok, State};
handle_call({set_pen_state, true}, _From, {T = #turtle{pen=false},D}) ->
	NT = T#turtle{pen=true},
	{reply, ok, {NT, add_circle(NT,D)}};

%% Pen colour
%% We do nothing if the old and new colours are the same.
handle_call({set_pen_colour, Colour}, _From, {#turtle{colour=Colour},_D}=State) ->
	{reply, ok, State};
handle_call({set_pen_colour, Colour}, _From, {T,D}) ->
	NT = T#turtle{colour=Colour},
	{reply, ok, {NT, add_circle(NT,D)}};

%% Bg colour
handle_call({set_bg_colour, Colour}, _From, {T,D}) ->
	{reply, ok, {T, D#drawing{bgcolour=Colour}}};

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
%% Internal functions - High level functionality
%%====================================================================

%% Heading is counted clockwise in degrees
%% 0 is at east (like in math).
-spec rotate(turtle(),angle_deg()) -> turtle().
rotate(Turtle = #turtle{heading=PrevHeading}, Angle) ->
	fixup_heading(Turtle#turtle{heading=PrevHeading+Angle}).

-spec set_heading(turtle(),angle_deg()) -> turtle().
set_heading(Turtle = #turtle{}, Angle) ->
	fixup_heading(Turtle#turtle{heading=Angle}).

-spec move(turtle(),drawing(),integer()) -> {turtle(),drawing()}.
move(Turtle=#turtle{},Drawing=#drawing{},Distance) ->
	Pos = calc_pos(Turtle,Distance),
	NT = Turtle#turtle{pos=Pos},
	{NT,move_draw(NT,Drawing)}.


%% Does the actual drawing part of the move (if pen is down)
-spec move_draw(turtle(),drawing()) -> drawing().
move_draw(#turtle{pen=false},Drawing=#drawing{}) ->
	Drawing;
move_draw(#turtle{pos=Pos,colour=Colour},Drawing=#drawing{nodes=[NH|NT]}) ->
	Circle={circle,Colour,Pos},
	case join_to_last(Circle,NH) of
		[Node] ->
			update_bounds(Drawing#drawing{nodes=[Node|NT]},Pos);
		[Node1,Node2] ->
			update_bounds(Drawing#drawing{nodes=[Node1,Node2|NT]},Pos)
	end.

-spec render(image_formats(),drawing()) ->
	{'raw',[tnode()]} | {'svg',iolist()}.
render(svg, Drawing) ->
	{svg,render_svg(Drawing)};
render(raw, #drawing{nodes=Nodes}) ->
	{raw,Nodes}.

%%====================================================================
%% Helper functions - Server - Heading and moving.
%%====================================================================

%% Ensure an angle in degrees is in "normal form" (0 <= a < 360).
-spec normalise_angle(angle_deg()) -> angle_deg().
normalise_angle(Heading) when Heading < 0 ->
	Heading rem 360 + 360;
normalise_angle(Heading) ->
	Heading rem 360.

%% Normalises heading and calculates the sin and cos values.
-spec fixup_heading(turtle()) -> turtle().
fixup_heading(Turtle = #turtle{heading=OldHeading}) ->
	NewHeading = normalise_angle(OldHeading),
	Radians = math:pi()/180 * NewHeading,
	Sin = math:sin(Radians),
	Cos = math:cos(Radians),
	Turtle#turtle{heading=NewHeading,sin=Sin,cos=Cos}.

%% Calculates where a move will end up
-spec calc_pos(turtle(),integer()) -> tcoord().
calc_pos(#turtle{sin=Sin,cos=Cos,pos={X,Y}},Distance) ->
	Dx = round(Cos * Distance),
	Dy = round(Sin * Distance),
	{X+Dx,Y+Dy}.

%% Extends the bounds to include a given point.
-spec update_bounds(drawing(),tcoord()) -> drawing().
update_bounds(Drawing=#drawing{bounds=none},{X,Y}) ->
	Drawing#drawing{bounds={{X,Y},{X,Y}}};
update_bounds(Drawing=#drawing{bounds={{LeftX,TopY},{RightX,BottomY}}},{X,Y}) ->
	NewLeftX   = erlang:min(LeftX, X),
	NewRightX  = erlang:max(RightX, X),
	NewTopY    = erlang:min(TopY, Y),
	NewBottomY = erlang:max(BottomY, Y),
	Drawing#drawing{bounds={{NewLeftX,NewTopY},{NewRightX,NewBottomY}}}.


%%====================================================================
%% Helper functions - Server - Drawing
%%====================================================================

%% Add a circle at current location if pen is down.
-spec add_circle(turtle(),drawing()) -> drawing().
add_circle(#turtle{pen=false}, Drawing = #drawing{}) ->
	Drawing;
add_circle(#turtle{pos=Pos,colour=Colour, pen=true}, Drawing = #drawing{nodes=N}) ->
	update_bounds(Drawing#drawing{nodes=[{circle,Colour,Pos}|N]},Pos).

%% Tries to join a new circle (N) with the previous circle/line (O) if possible.
%% If the colour of O and N are the same then it is straight-forward.
%% If they differ, then we add a new line from that last point to here.
%% Returns a list of 1 or 2 elements. If two, the top element will be first.
-spec join_to_last(circle(),circle()|line()) -> [tnode(),...].
join_to_last({circle,NCol,NPos},{circle,NCol,OPos}) ->
	% Replace with line
	[{line,NCol,[NPos,OPos]}];
join_to_last({circle,NCol,NPos},O={circle,_OCol,OPos}) ->
	% Add line but keep previous circle.
	[{line,NCol,[NPos,OPos]},O];
join_to_last({circle,NCol,NPos},{line,NCol,[_|_] = OList}) ->
	% Add to line.
	[{line,NCol,[NPos|OList]}];
join_to_last({circle,NCol,NPos},O={line,_OCol,[OH|_OT]}) ->
	% Add new line but keep previous line.
	[{line,NCol,[NPos,OH]},O].


%%====================================================================
%% Helper functions - Client
%%====================================================================


%% Throws an exception of a colour isn't valid.
-spec validate_colour(colour()) -> 'ok'.
validate_colour({R,G,B})
	when is_integer(R), is_integer(G), is_integer(B),
	     R >= 0, G >= 0, B >= 0, R =< 255, G =< 255, B =< 255 ->
	ok;
validate_colour({_,_,_}=C) ->
	throw({turt_invalid_colour,C}).


%%====================================================================
%% Rendering to SVG
%%====================================================================
-spec render_svg(drawing()) -> iolist().
render_svg(#drawing{nodes=Nodes,bounds=Bounds,bgcolour=Colour}) ->
	RenderedNodes = svg_render_nodes(Nodes, []),
	[svg_header(Bounds),svg_bg(Colour, Bounds),RenderedNodes,<<"</svg>\n">>].

%% Returns SVG bounds as, [X,Y,W,H] with padding applied
%% For empty drawing it returns all zeros.
-spec svg_bounds(tbounds()) -> [integer(),...].
svg_bounds(none) ->
	[0,0,0,0];
svg_bounds({{Left,Top},{Right,Bottom}}) ->
	[Left-1, Top-1, Right-Left+2, Bottom-Top+2].

%% Format the view box string, we use a padding of 1.
-spec svg_format_bounds(tbounds()) -> iolist().
svg_format_bounds(Bounds) ->
	io_lib:format("~p ~p ~p ~p", svg_bounds(Bounds)).

%% Generates the header (almost static, just viewbox that can vary)
-spec svg_header(tbounds()) -> iolist().
svg_header(Bounds) ->
	[<<"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n",
	   "<!-- Created with efunge -->\n",
	   "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n",
	   "<svg xmlns=\"http://www.w3.org/2000/svg\" baseProfile=\"full\" version=\"1.1\" viewBox=\"">>,
	 svg_format_bounds(Bounds),
	 <<"\">\n",
	   " <defs>\n",
	   "  <style type=\"text/css\">path{fill:none;stroke-width:0.5px;stroke-linecap:round;stroke-linejoin:round}</style>\n",
	   " </defs>\n">>].

-spec svg_render_nodes([tnode()],iolist()) -> iolist().
svg_render_nodes([], Output) ->
	Output;
svg_render_nodes([{line,Colour,PathNodes}|T], Output) ->
	S = [<<" <path d=\"">>,svg_render_path(PathNodes, []),
	     <<"\" style=\"stroke:">>,hexcolour(Colour),<<"\" />\n">>],
	svg_render_nodes(T, [S|Output]);
svg_render_nodes([{circle,Colour,{X,Y}}|T], Output) ->
	S = io_lib:format(" <circle cx=\"~p\" cy=\"~p\" fill=\"~s\" r=\"0.25\" />\n",
	                  [X,Y,hexcolour(Colour)]),
	svg_render_nodes(T, [S|Output]).

-spec svg_render_path([tcoord(),...],iolist()) -> iolist().
svg_render_path([], Output) ->
	throw({turt_empty_path,  [{output,Output}]});
%% Need special handling for first coordinate..., which is the last one
%% processed (to avoid reversing list at the end).
svg_render_path([{X,Y}], Output) ->
	Pos = io_lib:format("M~b,~b", [X,Y]),
	[Pos|Output];
svg_render_path([{X,Y}|T], Output) ->
	Pos = io_lib:format(" L~b,~b", [X,Y]),
	svg_render_path(T, [Pos|Output]).

-spec svg_bg(none|colour(),tbounds()) -> iolist().
svg_bg(none, _Bounds) -> [];
svg_bg(Colour, Bounds) ->
	[X,Y,W,H] = svg_bounds(Bounds),
	io_lib:format(" <rect x=\"~b\" y=\"~b\" width=\"~b\" height=\"~b\" style=\"fill:~s;stroke:none\" />\n",
	              [X,Y,W,H,hexcolour(Colour)]).

%%====================================================================
%% Helper functions - Rendering
%%====================================================================

-spec hexcolour(colour()) -> iolist().
hexcolour({R,G,B}) ->
	io_lib:format("#~2.16.0b~2.16.0b~2.16.0b", [R,G,B]).


%% Newest node should be first!
-spec prune_circles(drawing()) -> drawing().
prune_circles(D=#drawing{nodes=Nodes}) ->
	NewNodes = prune_circles(Nodes, [], sets:new()),
	D#drawing{nodes=NewNodes}.

-spec add_line_points([tcoord()],set()) -> set().
add_line_points([], Coords) ->
	Coords;
add_line_points([H|T], Coords) ->
	add_line_points(T, sets:add_element(H,Coords)).

-spec prune_circles([tnode()],[tnode()],set()) -> [tnode()].
prune_circles([], Output, _Coords) ->
	lists:reverse(Output);
prune_circles([H={circle,_Colour,Pos}|T], Output, Coords) ->
	case sets:is_element(Pos,Coords) of
		true ->
			prune_circles(T, Output, Coords);
		false ->
			prune_circles(T, [H|Output], sets:add_element(Pos,Coords))
	end;
prune_circles([H={line,_Colour,PosList}|T], Output, Coords) ->
	prune_circles(T, [H|Output], add_line_points(PosList,Coords)).
