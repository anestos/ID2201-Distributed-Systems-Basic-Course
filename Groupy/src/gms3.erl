-module(gms3).
-export([start/1, start/2]).

-define(timeout, 1000).
-define(arghh, 100).

% Starting the leader
start(Id) ->
	Rnd = random:uniform(1000),
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

% Leader Initialization
init(Id,Rnd, Master) ->
	random:seed(Rnd, Rnd, Rnd),
	leader(Id, Master, 1, [], [Master]).

% Starting the Slave
start(Id, Grp) ->
	Rnd = random:uniform(1000),	
	Self = self(),
	{ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.

% Slave Initialization
init(Id, Rnd, Grp, Master) ->
	Self = self(),
	random:seed(Rnd, Rnd, Rnd),	
	Grp ! {join, Master, Self},
	receive
		{view, N, [Leader|Slaves], Group} ->
			Master ! {view, Group},
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves], Group}, Slaves, Group)
	after ?timeout ->
		Master ! {error, "no reply from leader"}
	end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
	%io:format("Slave ~w ~n",[Id]),
	receive
		{mcast, Msg} ->
			io:format("gms ~w: received {mcast, ~w} in state ~w~n", [Id, Msg, N]),
			Leader ! {mcast, Msg},
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		{join, Wrk, Peer} ->
			io:format("gms ~w: forward join from ~w to leader~n", [Id, Peer]),
			Leader ! {join, Wrk, Peer},
			slave(Id, Master, Leader, N, Last, Slaves, Group);
		{msg, I, _} when I < N ->
			slave(Id,Master,Leader,N,Last,Slaves,Group);
		{msg, N, Msg} ->
			io:format("gms ~w: deliver msg ~w in state ~w~n", [Id, Msg, N]),
			Master ! Msg,
			slave(Id, Master, Leader, N+1, {msg, N, Msg}, Slaves, Group);
		{view, N, [Leader|Slaves2], Group2} ->
			io:format("gms ~w: received view ~w ~w~n", [Id, N, view]),
			Master ! {view, Group2},
			slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves2], Group2}, Slaves2, Group2);
		stop ->
			ok;
		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Id, Master, N, Last, Slaves, Group);
		Error ->
			io:format("gms ~w: slave, strange message ~w~n", [Id, Error])
	end.

election(Id, Master, N, Last, Slaves, [_|Group]) ->
	%io:format("election~n",[]),
	Self = self(),
	case Slaves of
		[Self|Rest] ->
			bcast(Id, Last, Rest),			
			bcast(Id, {view, N, Slaves, Group}, Rest),
			Master ! {view, N, Group},
			leader(Id, Master, N+1, Rest, Group);
		[Leader|Rest] ->
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, N, Last, Rest, Group)
	end.

leader(Id, Master, N, Slaves, Group) ->
	%io:format("Leader ~w ~n",[Id]),
	receive
		{mcast, Msg} ->
			%%io:format("gms ~w: received {mcast, ~w} in state ~w~n", [Id, Msg, N]),
			bcast(Id, {msg, N, Msg}, Slaves),
			Master ! Msg,
			leader(Id, Master, N+1, Slaves, Group);
		{join, Wrk, Peer} ->
			%%io:format("gms ~w: forward join from ~w to master~n", [Id, Peer]),
			Slaves2 = lists:append(Slaves, [Peer]),
			Group2 = lists:append(Group, [Wrk]),
			bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
			Master ! {view, Group2},
			leader(Id, Master, N+1, Slaves2, Group2);
		stop ->
			ok;
		Error ->
			io:format("gms ~w: leader, strange message ~w~n", [Id, Error])
	end.

bcast(Id, Msg, Nodes) ->
	lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id)->
	case random:uniform(?arghh) of 
		?arghh ->
			io:format("leader ~w: crash ~n",[Id]),
			exit(no_luck);
		_->
			ok
	end.