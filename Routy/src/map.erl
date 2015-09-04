-module(map).
-export([new/0,update/3,reachable/2,all_nodes/1]).

%returns an empty map (a empty list)
new() -> [].

%updates the Map to reflect that Node has directional
%links to all nodes in the list Links. The old entry is removed.
update(Node, Links, Map) -> 
	case lists:keysearch(Node, 1, Map) of
		{value, {_, _}} ->
			[{Node, Links}|lists:keydelete(Node, 1, Map)];
		false ->
			[{Node, Links}|Map]
	end.

%returns the list of nodes directly reachable from Node.
reachable(Node, Map) -> 
	case lists:keyfind(Node,1,Map) of
		false -> [];
		{_, ListNodes} -> ListNodes
	end.

%returns a list of all nodes in the map, also the ones
%without outgoing links. So if berlin is linked to london but london
%does not have any outgoing links (and thus no entry in the list), berlin
%should still be in the returned list.
all_nodes(Map) ->
	all_nodes(Map,[]).

all_nodes(Map,Nodes)->
	case Map of
		[]->
			lists:usort(Nodes);
		[{City, Links}|T]->
			all_nodes(T,[City] ++ Links ++ Nodes)
	end.