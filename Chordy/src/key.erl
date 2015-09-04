-module(key).
-export([generate/0,between/3]).

generate()->
	random:uniform(1000000000).
%return true false
between(Key,From,To)->
	if From > To ->
		   Key > From orelse Key =< To;
	   From < To ->
		   Key > From andalso Key =< To;
	   true ->
		   true
	end.