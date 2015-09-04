-module(test).
-export([startn1/1,startn2/1,startn3/1,asdf/0]).

startn1(Number) ->
	NodeZero = node1:start(),
	register(node1, NodeZero),
	node1 ! stabilize,
	start_nodes(NodeZero, Number+1).

start_nodes(NodeZero,Number) ->
	case Number > 1 of 
		true ->	
			%Id = key:generate(),
			Id = Number,
			register(list_to_atom("node" ++ integer_to_list( Number )), node1:start(Id,node1)),
			%%list_to_atom("node" ++ integer_to_list( Number )) ! stabilize,
			start_nodes(NodeZero, Number-1);
		false->
			"nodes started"
	end.

startn2(Number) ->
	NodeZero = node2:start(),
	register(node1, NodeZero),
	node1 ! stabilize,
	start_nodes2(NodeZero, Number+1).

start_nodes2(NodeZero,Number) ->
	case Number > 1 of 
		true ->	
			%Id = key:generate(),
			Id = Number*100,
			register(list_to_atom("node" ++ integer_to_list( Number )++"00"), node2:start(Id,node1)),
			%%list_to_atom("node" ++ integer_to_list( Number )++"00") ! stabilize,
			start_nodes2(NodeZero, Number-1);
		false->
			"nodes started"
	end.

startn3(Number) ->
	NodeZero = node3:start(),
	register(node1, NodeZero),
	node1 ! stabilize,
	start_nodes3(NodeZero, Number+1).

start_nodes3(NodeZero,Number) ->
	case Number > 1 of 
		true ->	
			%Id = key:generate(),
			Id = Number*100,
			register(list_to_atom("node" ++ integer_to_list( Number )++"00"), node3:start(Id,node1)),
			%%list_to_atom("node" ++ integer_to_list( Number )++"00") ! stabilize,
			start_nodes3(NodeZero, Number-1);
		false->
			"nodes started"
	end.

asdf()->
	register(node1,node4:start()),
	register(node10,node4:start(10,node1)),
	register(node20,node4:start(20,node1)),
	register(node30,node4:start(30,node1)),
	register(node40,node4:start(40,node1)),
	register(node50,node4:start(50,node1)),
	register(node60,node4:start(60,node1)),
	register(node70,node4:start(70,node1)),
	register(node80,node4:start(80,node1)),
	register(node90,node4:start(90,node1)),
	register(node100,node4:start(100,node1)),
	register(node110,node4:start(110,node1)),
	register(node120,node4:start(120,node1)),
	register(node130,node4:start(130,node1)).


