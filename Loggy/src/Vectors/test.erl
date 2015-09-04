-module(test).
-export([run/2]).
run(Sleep, Jitter) ->
	Workers = [a, b, c, d],
	Log = logger:start(Workers),
	A = worker:start(john, Log, 13, Sleep, Jitter,Workers),
	B = worker:start(paul, Log, 23, Sleep, Jitter,Workers),
	C = worker:start(ringo, Log, 36, Sleep, Jitter,Workers),
	D = worker:start(george, Log, 49, Sleep, Jitter,Workers),
	worker:peers(A, [B, C, D]),
	worker:peers(B, [A, C, D]),
	worker:peers(C, [A, B, D]),
	worker:peers(D, [A, B, C]),
	timer:sleep(5000),
	logger:stop(Log),
	worker:stop(A),
	worker:stop(B),
	worker:stop(C),
	worker:stop(D).
