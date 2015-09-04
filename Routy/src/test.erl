%% @author Nikos
%% @doc @todo Add description to test.

-module(test).
-export([start/0,stop/0,test/0]).

start() ->
	routy:start(stockholm),
	routy:start(lund),
	routy:start(uppsala),
	%routy:start(kista),
	%routy:start(solna),
	%routy:start(rinkeby),
	
	%stockholm ! {add, kista, {kista, 'nikos@kth'}},
	%kista ! {add, stockholm, {stockholm, 'nikos@kth'}},
	stockholm ! {add, lund, {lund, 'nikos@kth'}},
	%lund ! {add, stockholm, {stockholm, 'nikos@kth'}},
	lund ! {add, uppsala, {uppsala, 'nikos@kth'}},
	stockholm ! {add, uppsala, {uppsala, 'nikos@kth'}},
	%uppsala ! {add, stockholm, {stockholm, 'nikos@kth'}},	
	%uppsala ! {add, lund, {lund, 'nikos@kth'}},	
	%rinkeby ! {add, kista, {stockholm, 'nikos@kth'}},
	%solna ! {add, kista, {kista, 'nikos@kth'}},
	%uppsala ! {add, rinkeby, {rinkeby, 'nikos@kth'}},
	%uppsala ! {add, solna, {solna, 'nikos@kth'}},
	
	stockholm ! broadcast,
	
	
	started.

stop()->
	routy:stop(stockholm),
	routy:stop(lund),
	routy:stop(uppsala),
	%	routy:stop(kista),
	%	routy:stop(solna),
	%	routy:stop(rinkeby),
	stopped.

ip() -> 'nikos@kth'.
test() ->
	routy:start(copenhagen),
	io:format("[Test] Started router 'copenhagen'~n", []),
	routy:start(odense),
	io:format("[Test] Started router 'odense'~n", []),
	copenhagen ! {add, odense, {odense, ip()}},
	io:format("[Test] Added 'odense' to 'copenhagen'~n", []),
	odense ! {add, copenhagen, {copenhagen, ip()}},
	io:format("[Test] Added 'copenhagen' to 'odense'~n", []),
	copenhagen ! {add, kumla, {kumla, ip()}},
	io:format("[Test] Added 'copenhagen' to 'kumla'~n", []),
	copenhagen ! broadcast,
	timer:sleep(100),
	odense ! broadcast,
	timer:sleep(100),
	copenhagen ! update,
	odense ! update,
	copenhagen ! {status, self()},
	timer:sleep(100),
	odense ! {status, self()},
	timer:sleep(100).
