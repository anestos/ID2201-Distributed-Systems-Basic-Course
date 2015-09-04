-module(logger).
-export([start/1, stop/1]).
start(Nodes) ->
	spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
	io:format("~nLogger shutdown ~n~n", []),
	
	Logger ! stop.

loop(MessageQueue,NodeList) ->
	receive
		{log, From, Time, Msg} ->
			io:format("MsgReceived~n", []),
			UpdateNodeList = lists:keyreplace(From, 1, NodeList, {From, Time}),
			Sorted = lists:keysort(2,MessageQueue++[{From, Time, Msg}]),
			log(Sorted,UpdateNodeList);
		stop ->
			dump(MessageQueue),
			ok
	after 
		500 ->
		io:format("TimedOut~n", []),
		log(MessageQueue,NodeList)
	end.

log(MessageQueue,NodeList)->
	SafeToPrint = find_safe_to_print(NodeList, MessageQueue),
	{Printed, RestQueue} = lists:split(SafeToPrint, MessageQueue),
	dump(Printed),
	loop(RestQueue,NodeList).

find_safe_to_print(NodeList, MessageQueue) ->
	[{_,LowestTimestamp}|_] = lists:keysort(2,NodeList),
	messages_to_print(LowestTimestamp,0,MessageQueue).

messages_to_print(Max,N,Queue) ->
	case Queue of 
		[]->
			N;
		[{_,X,_}|T] ->
			case Max >= X of 
				true ->
					messages_to_print(Max,N+1,T);
				false->
					N
			end
	end.

dump(List) ->
	case List of 
		[]->
			ok;
		[{From,Time,Msg}|T]->
			io:format("log: ~w ~w ~p~n", [From, Time, Msg]),
			dump(T)
	end.


init(Nodes) ->
	NodeList = [{Node, 0} || Node <- Nodes],
	loop([],NodeList).
