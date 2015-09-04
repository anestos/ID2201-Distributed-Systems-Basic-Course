-module(node4).
-compile(export_all).
-define('Stabilize',1000).
-define('Timeout',1000).

start() ->
	Id = 1,
	start(Id, nil).

start(Id, Peer) ->
	timer:start(),
	spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
	Predecessor = nil,
	Next = nil,
	{ok, {Skey,SPeer}} = connect(Id, Peer),
	Successor = {Skey,monitor(SPeer),SPeer},
	schedule_stabilize(),
	node(Id, Predecessor, Successor, Next).

connect(Id, nil) ->
	{ok, {Id,self()}}; 

connect(_Id, Peer) ->
	Qref = make_ref(),
	Peer ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			{ok,{Skey,Peer}}
	after ?Timeout ->
		io:format("Time out: no response~n",[])
	end.

node(Id, Predecessor, Successor, Next) ->
	receive
		{key, Qref, Peer} ->
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor, Next);
		{notify, New} ->
			Pred = notify(New, Id, Predecessor),
			node(Id, Pred, Successor, Next);
		{request, Peer} ->
			request(Peer, Predecessor, Next),
			node(Id, Predecessor, Successor, Next);
		{status, Pred, Nx} ->
			{Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
			node(Id, Predecessor, Succ, Nxt);
		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor, Next);
		probe ->
			create_probe(Id, Successor),
			node(Id, Predecessor, Successor, Next);
		{probe, Id, Nodes, T} ->
			remove_probe(T, Nodes),
			node(Id, Predecessor, Successor, Next);
		{probe, Ref, Nodes, T} ->
			forward_probe(Ref, T, Nodes, Id, Successor),
			node(Id, Predecessor, Successor, Next);
		status ->
			io:format("NodeID= ~w   Pred=~w Succ=~w  Next:~w ~n", [Id,Predecessor,Successor, Next]),
			node(Id, Predecessor, Successor, Next);
		{'DOWN', Ref, process, _, _} ->
			{Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next, Id),
			node(Id, Pred, Succ, Nxt);
		stop ->
			stop
	end.

schedule_stabilize() ->
	timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, _, Spid}) ->
	Spid ! {request, self()}.

%id 10
%now returns {Succ,Nxt}
%Pred = our successor's predecesor 
stabilize(Pred, Next, Id, Successor) ->

	{Skey,_, Spid} = Successor, %20
	case Pred of
		nil ->
			Spid ! {notify, {Id, self()}},
			{Successor,Next};
		{Id, _} ->
			{Successor,Next};
		{Skey, _} ->
			Spid ! {notify, {Id,self()}},
			{Successor,Next};
		{Xkey, Xpid} -> %15
			case key:between(Xkey, Id, Skey) of
				true ->
					Xpid ! {request, self()},
					{{Xkey,monitor(Xpid), Xpid}, {Skey,Spid}};
				false ->
					Spid ! {notify, {Id, self()}},
					{Successor,Next}
			end
	end.

request(Peer, Predecessor, Next) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil, Next};
		{Pkey,_, Ppid} ->
			Peer ! {status, {Pkey, Ppid}, Next}
	end.

notify({Nkey, Npid}, Id, Predecessor) ->
	case Predecessor of
		nil ->
			{Nkey, monitor(Npid), Npid};
		{Pkey,Pref, _} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					demonitorz(Pref),
					{Nkey, monitor(Npid), Npid}; 
				false ->
					Predecessor 
			end
	end.

create_probe(Id, Successor) ->
	{_,_,Pid} = Successor,
	Pid ! {probe, Id,[Id],erlang:now()}.

remove_probe(T, Nodes) ->
	Diff = timer:now_diff(erlang:now(), T),
	L = lists:flatlength(Nodes),
	io:format("Node:~w Removing probe after ~wmicros?.~n Nodes visited: ~w~n~n", [self(), Diff, L]).

forward_probe(Ref, T, Nodes, Id, Successor)->
	{_,_,Pid} = Successor,
	io:format("Node:~w forwarding probe to ~w ~n",[Id,Pid]),
	Pid ! {probe,Ref,Nodes ++ [Id],T}.



monitor(Pid) ->
	erlang:monitor(process, Pid).
demonitorz(nil) ->
	ok;
demonitorz(Pid) ->
	erlang:demonitor(Pid, [flush]).

down(Ref, {_, Ref, _}, Successor, Next,_) ->
	{nil, Successor, Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid},Id) ->
	Nref = monitor(Npid),
	Npid ! {notify, {Id, self()}},
	{Predecessor, {Nkey, Nref, Npid}, nil}.
