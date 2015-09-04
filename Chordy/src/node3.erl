-module(node3).
-compile(export_all).
-define('Stabilize',500). 
-define('Timeout',1000).

start() ->
	%Id = key:generate(),
	Id = 1,
	start(Id, nil).

start(Id, Peer) ->
	timer:start(),
	spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
	Predecessor = nil,
	{ok, {Skey,SPeer}} = connect(Id, Peer),
	Successor = {Skey,monitor(SPeer),SPeer},
	schedule_stabilize(),
	Store = storage:create(),
	Next = nil,
	node(Id, Predecessor, Successor, Store, Next).

connect(Id, nil) ->
	{ok, {Id,self()}}; %first node, set our successor pointer to ourself

connect(_Id, Peer) ->
	Qref = make_ref(),
	Peer ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			{ok,{Skey,Peer}} 
	after ?Timeout ->
		io:format("Time out: no response~n",[])
	end.

node(Id, Predecessor, Successor, Store, Next) ->
	receive
		{key, Qref, Peer} ->
			%io:format("Received key command~n---~nQref= ~w Peer= ~w ~n---~n ", [Qref,Peer]),
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor, Store, Next);
		{notify, New} ->
			%io:format("Received notify command~n---~n New= ~w ~n---~n ", [New]),
			{Pred, KeepStore} = notify(New, Id, Predecessor, Store),
			node(Id, Pred, Successor, KeepStore, Next);
		{request, Peer} ->
			%io:format("ID: ~w   Received REQUEST command  Peer= ~w ~n ", [Id,Peer]),
			request(Peer, Predecessor, Next),
			node(Id, Predecessor, Successor, Store, Next);
		{status, Pred, Nx} ->
			{Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
			node(Id, Predecessor, Succ, Store, Nxt);
		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor, Store, Next);
		probe ->
			create_probe(Id, Successor),
			node(Id, Predecessor, Successor, Store, Next);
		{probe, Id, Nodes, T} ->
			remove_probe(T, Nodes),
			node(Id, Predecessor, Successor, Store, Next);
		{probe, Ref, Nodes, T} ->
			forward_probe(Ref, T, Nodes, Id, Successor, Store),
			node(Id, Predecessor, Successor, Store, Next);
		status ->
			io:format("NodeID= ~w   Pred=~w Succ=~w Next= ~w Storage: ~w~n", [Id,Predecessor,Successor, Next, Store]),
			node(Id, Predecessor, Successor, Store, Next);
		%%node2
		{add, Key, Value}->
			self() ! {add, Key, Value, self(), self()};
		{add, Key, Value, Qref, Client} ->
			Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Added, Next);
		{lookup, Key, Qref, Client} ->
			lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
			node(Id, Predecessor, Successor, Store, Next);
		{handover, Elements} ->
			Merged = storage:merge(Store, Elements),
			node(Id, Predecessor, Successor, Merged, Next);
		{'DOWN', Ref, process, _, _} ->
			{Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
			node(Id, Pred, Succ, Store, Nxt);
		
		
		stop ->
			stop
	end.

schedule_stabilize() ->
	timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_,_, Spid}) ->
	Spid ! {request, self()}.

%% The periodic stabilize procedure will consist of a node sending a {request,
%% self()} message to its successor and then expecting a {status, Pred} in
%% return. When it knows the predecessor of its successor it can check if the ring
%% is stable or if the successor needs to be notifies about its existence through
%% a {notify, {Id, self()} message.
stabilize(Pred, Next, Id, Successor) ->
	{Skey, Sref, Spid} = Successor,
	case Pred of
		nil ->
			%we should of course inform it about our existence
			Spid ! {notify, {Id, self()}},
			{Successor, Next};
		{Id,_, _} ->
			%it is pointing back to us we dont have to do anything
			{Successor, Next}; 
		{Skey,_, _} ->
			%it is pointing to itself we should of course notify it about our existence
			Spid ! {notify, {Id,self()}},
			{Successor, Next}; 
		{Xkey, _ , Xpid} ->
			case key:between(Xkey, Id, Skey) of
				true ->
					%If we should be in between the nodes we inform our successor of our existence.
					Xpid ! {request, self()}, 
					{{Xkey, monitor(Xpid), Xpid}, Next}; 
				false ->
					%adopt this node as our successor and run stabilization again
					Spid ! {notify, {Id, self()}},
					{Successor, Next}
			end
	end.


request(Peer, Predecessor, Next) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil, Next};
		{Pkey, Pref, Ppid} ->
			Peer ! {status, {Pkey, Pref, Ppid}, Next}
	end.

%returns Predecessor
notify({Nkey, Npid}, Id, Predecessor, Store) ->
	case Predecessor of
		nil ->
			Keep = handover(Store, Nkey, Npid),
			{{Nkey, monitor(Npid), Npid}, Keep}; %If our own predecessor is set to nil the case is closed (set it to the new guy)
		{Pkey,_, _} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					Keep = handover(Store, Nkey, Npid),
					{{Nkey, monitor(Npid), Npid}, Keep}; 
				false ->
					{Predecessor, Store} 
			end
	end.

%send first probe
create_probe(Id, Successor) ->
	{_,_,Pid} = Successor,
	Pid ! {probe, Id,[Id],erlang:now()}.
%print Time
remove_probe(T, Nodes) ->
	Diff = timer:now_diff(erlang:now(), T),
	L = lists:flatlength(Nodes),
	io:format("Node:~w Removing probe after ~wmicros?.~n Nodes visited: ~w~n~n", [self(), Diff, L]).

%send prob to successor
forward_probe(Ref, T, Nodes, Id, Successor , MyStore)->
	{_,_,Pid} = Successor,
	io:format("Node:~w forwarding probe to ~w  Storage: ~w~n",[Id,Pid,MyStore]),
	Pid ! {probe,Ref,Nodes ++ [Id],T}.


%node2
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, _,Spid}, Store) ->
	case key:between(Key, Pkey, Id) of
		true ->
			Client ! {Qref, ok},
			storage:add(Store,Key, Value);
		false ->
			Spid ! {add, Key, Value, Qref, Client},
			Store
	end.

%node2
lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
	case key:between(Key, Pkey, Id) of
		true ->
			Result = storage:lookup(Key, Store),
			Client ! {Qref, Result};
		false ->
			{_, Spid} = Successor,
			Spid ! {lookup, Key, Qref, Client}
	end.

%node2
handover(Store, Nkey, Npid) ->
	{Keep, Leave} = storage:split(Nkey, Store),
	Npid ! {handover, Leave},
	Keep.
%% node3
monitor(Pid) ->
	erlang:monitor(process, Pid).
demonitorz(nil) ->
	ok;
demonitorz(Pid) ->
	erlang:demonitor(Pid, [flush]).

down(Ref, {_, Ref, _}, Successor, Next) ->
	{nil, Successor, Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
	demonitorz(Ref),
	Nref = monitor(Npid),
	Npid ! {notify, {Nkey, self()}},
	{Predecessor, {Nkey, Nref, Npid}, nil};
down(A,B,C,D)->
io:format("A:~w B:~w C:~w D:~w~nnn", [A,B,C,D]).

