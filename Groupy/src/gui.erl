-module(gui).
-include_lib("wx/include/wx.hrl").
-define(width, 200).
-define(height, 200).
-define(wxWhite, '255,255,255').

-export([start/2]).
start(Title, Master) ->
	spawn_link(fun() -> init(Title, Master) end).
init(Title, Master) ->
	Window = make_window(Title),
	loop(Window, Master).
make_window(Title) ->
	Server = wx:new(), %Server will be the parent for the Frame
	Frame = wxFrame:new(Server, -1, Title, [{size,{?width, ?height}}]),
	wxFrame:setBackgroundColour(Frame, ?wxWHITE),
	Window = wxWindow:new(Frame, ?wxID_ANY),
	wxFrame:show(Frame),
	wxWindow:setBackgroundColour(Window, ?wxWHITE),
	wxWindow:show(Window),
	%% monitor closing window event
	wxFrame:connect(Frame, close_window),
	Window.
loop(Window, Master)->
	receive
		%% check if the window was closed by the user
		#wx{event=#wxClose{}} ->
			wxWindow:destroy(Window),
			Master ! stop,
			ok;
		{color, Color} ->
			color(Window, Color),
			%io:format("change color~w~n", [Color]),
			wxWindow:refresh(Window),
			loop(Window, Master);
		stop ->
			ok;
		Error ->
			io:format("gui: strange message ~w ~n", [Error]),
			loop(Window, Master)
	end.
color(Window, Color) ->
	wxWindow:setBackgroundColour(Window, Color).
