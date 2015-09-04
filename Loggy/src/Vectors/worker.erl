-module(worker).
-export([start/6, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter,Workers) ->
	spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter,Workers) end).

stop(Worker) ->
	Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter, Workers) ->
	random:seed(Seed, Seed, Seed),
	Lamport = [{Node, 0} || Node <- Workers],
	receive
		{peers, Peers} ->
			loop(Name, Log, Peers, Sleep, Jitter, Lamport);
		stop ->
			ok
	end.

peers(Wrk, Peers) ->
	Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Lamport)->
	Wait = random:uniform(Sleep),
	receive
		{msg, From, LamportClock, Msg} ->
			{_,Time} = lists:keyfind(From, 1, LamportClock), %myLamportClock
			
			MyLamportClock = lists:keyreplace(From, 1, Lamport, {From, Time}),
			Log ! {log, Name, MyLamportClock, {received, Msg}},
			loop(Name, Log, Peers, Sleep, Jitter, MyLamportClock);
		stop ->
			ok;
		Error ->
			Log ! {log, Name, time, {error, Error}}
	after Wait ->
		Selected = select(Peers),
		
		{_,Time} = lists:keyfind(Name, 1, Lamport), %myLamportClock
		UpdatedLamportClock = lists:keyreplace(Name, 1, Lamport, {Name, Time+1}),
		
		Message = {hello, Name, random:uniform(100)+Time},
		Selected ! {msg, Name, UpdatedLamportClock, Message},
		jitter(Jitter),
		Log ! {log, Name, UpdatedLamportClock, {sending, Message}},
		loop(Name, Log, Peers, Sleep, Jitter, UpdatedLamportClock)
	end.

select(Peers) ->
	lists:nth(random:uniform(length(Peers)), Peers).
jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
