-module(node1).
-compile(export_all).
-define('Stabilize',5000).
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
	{ok, Successor} = connect(Id, Peer),
	schedule_stabilize(),
	node(Id, Predecessor, Successor).

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

node(Id, Predecessor, Successor) ->
	receive
		{key, Qref, Peer} ->
			%io:format("Received key command~n---~nQref= ~w Peer= ~w ~n---~n ", [Qref,Peer]),
			Peer ! {Qref, Id},
			node(Id, Predecessor, Successor);
		{notify, New} ->
			%io:format("Received notify command~n---~n New= ~w ~n---~n ", [New]),
			Pred = notify(New, Id, Predecessor),
			node(Id, Pred, Successor);
		{request, Peer} ->
			%io:format("ID: ~w   Received REQUEST command  Peer= ~w ~n ", [Id,Peer]),
			request(Peer, Predecessor),
			node(Id, Predecessor, Successor);
		{status, Pred} ->
			%io:format("ID: ~w    Received STATUS command Pred= ~w ~n ", [Id,Pred]),
			Succ = stabilize(Pred, Id, Successor),
			node(Id, Predecessor, Succ);
		stabilize ->
			stabilize(Successor),
			node(Id, Predecessor, Successor);
		probe ->
			create_probe(Id, Successor),
			node(Id, Predecessor, Successor);
		{probe, Id, Nodes, T} ->
			remove_probe(T, Nodes),
			node(Id, Predecessor, Successor);
		{probe, Ref, Nodes, T} ->
			forward_probe(Ref, T, Nodes, Id, Successor),
			node(Id, Predecessor, Successor);
		status ->
				io:format("NodeID= ~w   Pred=~w Succ=~w ~n", [Id,Predecessor,Successor]),
			node(Id, Predecessor, Successor);
		stop ->
			stop
	end.

schedule_stabilize() ->
	timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
	Spid ! {request, self()}.

%% The periodic stabilize procedure will consist of a node sending a {request,
%% self()} message to its successor and then expecting a {status, Pred} in
%% return. When it knows the predecessor of its successor it can check if the ring
%% is stable or if the successor needs to be notifies about its existence through
%% a {notify, {Id, self()} message.
stabilize(Pred, Id, Successor) ->
	{Skey, Spid} = Successor,
	case Pred of
		nil ->
			%we should of course inform it about our existence
			Spid ! {notify, {Id, self()}},
			Successor;
		{Id, _} ->
			%it is pointing back to us we don’t have to do anything.
			Successor; 
		{Skey, _} ->
			%it is pointing to itself we should of course notify it about our existence
			Spid ! {notify, {Id,self()}},
			Successor; 
		{Xkey, Xpid} ->
			case key:between(Xkey, Id, Skey) of
				true ->
					%If we should be in between the nodes we inform our successor of our existence.
					Xpid ! {request, self()}, 
					{Xkey, Xpid};
				false ->
					%adopt this node as our successor and run stabilization again
					Spid ! {notify, {Id, self()}},
					Successor
			end
	end.


request(Peer, Predecessor) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil};
		{Pkey, Ppid} ->
			Peer ! {status, {Pkey, Ppid}}
	end.

%returns Predecessor
notify({Nkey, Npid}, Id, Predecessor) ->
	case Predecessor of
		nil ->
			{Nkey, Npid}; %If our own predecessor is set to nil the case is closed (set it to the new guy)
		{Pkey, _} ->
			case key:between(Nkey, Pkey, Id) of
				true ->
					{Nkey,Npid}; %
				false ->
					Predecessor %
			end
	end.

%send first probe
create_probe(Id, Successor) ->
	{_,Pid} = Successor,
	Pid ! {probe, Id,[Id],erlang:now()}.
%print Time
remove_probe(T, Nodes) ->
  Diff = timer:now_diff(erlang:now(), T),
  L = lists:flatlength(Nodes),
  io:format("Node:~w Removing probe after ~wmicros?.~n Nodes visited: ~w~n~n", [self(), Diff, L]).

%send prob to successor
forward_probe(Ref, T, Nodes, Id, Successor)->
	{_,Pid} = Successor,
	io:format("Node:~w forwarding probe to ~w ~n",[Id,Pid]),
	Pid ! {probe,Ref,Nodes ++ [Id],T}.


