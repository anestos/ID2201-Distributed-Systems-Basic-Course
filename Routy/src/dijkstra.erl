-module(dijkstra).
-export([table/2,route/2,iterate/3,initSortedList/3]).

%returns the length of the shortest path to the node or 0 if the node is not found
entry(Node, Sorted) ->
	case lists:keysearch(Node, 1, Sorted) of
		{value, {Node, N, _}} ->
			N;
		false  ->
			0
	end.

%replaces the entry for Node in Sorted with a new entry having a new length N and Gateway.
%The resulting list should of course be sorted.
replace(Node, Length, Gateway, SortedList) ->	
	UnsortedList = lists:keyreplace(Node, 1, SortedList, {Node, Length, Gateway}),
	lists:keysort(2, UnsortedList).

%update the list Sorted given the information that Node can be reached in N hops using Gateway.
%If no entry is found then no new entry is added. Only if we have a
%better (shorter) path should we replace the existing entry.
update(Node, N, Gateway, Sorted) ->
	Length = entry(Node, Sorted),
	if 
		N < Length ->
			replace(Node,N,Gateway,Sorted);
		true -> Sorted
	end.

%construct a table given a sorted list of nodes, a map and a table constructed so far.
iterate(Sorted, Map, Table)->
	case Sorted of
		% If there are no more entries in the sorted list then we are done and the
		% given routing table is complete.
		[]->
			Table;
		
		% If the first entry is a dummy entry with an infinite path to a city we
		% know that the rest of the sorted list is also of infite length and the
		% given routing table is complete.
		[{_,inf,_}|_]->
			Table;
		
		% Otherwise, take the first entry in the sorted list, find the nodes in the
		% map reachable from this entry and for each of these nodes update the
		% Sorted list. The entry that you took from the sorted list is added to
		% the routing table		
		[H|T]->
			{Node,Hops,Gateway} = H,
			
			case map:reachable(Node, Map) of
				[] ->
					% No reachable nodes from current entry.
					% Prepend the table and continue iteration
					iterate(T, Map, [{Node, Gateway}] ++ Table);
				ReachableNodes ->
					% Found reachable nodes, update and put the result into UpdatedList
					UpdatedList = lists:foldl(
									fun(X, AccIn) ->
											update(X, Hops + 1, Gateway, AccIn)
									end, T, ReachableNodes
											 ),
					% Prepend the table and continue iteration
					iterate(UpdatedList, Map, [{Node, Gateway}] ++ Table)
			end
	end.

% construct a table given a sorted list
% of nodes, a map and a table constructed so far.
table(Gateways, Map) ->
	Nodes = map:all_nodes(Map),
	Sorted = initSortedList(Nodes,[],Gateways),
	iterate(Sorted,Map,[]).

initSortedList(List,NewList,Gateways)->
	case List of
		[]->
			lists:keysort(2, NewList);
		[H|T] ->
			case lists:member(H,Gateways) of 
				true ->
					initSortedList(T,[{H,0,H}]++NewList,Gateways);
				false->
					initSortedList(T,[{H,inf,unknown}]++NewList,Gateways)
			end
	end.

% search the routing table and return the gateway
% suitable to route messages to a node. If a gateway is found we should
% return {ok, Gateway} otherwise we return notfound.
route(Name, Table) ->
	case lists:keyfind(Name,1,Table) of
		{Name, Gateway} ->
			{ok, Gateway};
		false ->
			notfound
	end.