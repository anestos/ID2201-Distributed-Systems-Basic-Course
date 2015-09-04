-module(logger).
-export([start/1, stop/1]).
start(Nodes) ->
	spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
	io:format("~nLogger shutdown ~n~n", []),
	
	Logger ! stop.

loop(MessageQueue, NodeList) ->
	receive
		{log, From, VectorClock, Msg} ->
			io:format("MsgLogged~n", []),
			%% Logger Internal Clock
			{_,Time} = lists:keyfind(From, 1, VectorClock),
			UpdateNodeList = lists:keyreplace(From, 1, NodeList, {From, Time}),
			
			Sorted = MessageQueue++[{From, VectorClock, Msg}],
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
	{Printme, RestQueue} = find_safe_to_print(MessageQueue),
	dump(Printme),
	loop(RestQueue,NodeList).

find_safe_to_print(MessageQueue) ->
	find_safe_to_print(MessageQueue,[],[]).

find_safe_to_print(MessageQueue,ListToPrint,ListToQueue)->
	%george [{john,0},{paul,0},{ringo,2},{george,0}] {received,{hello,ringo,21}
	case MessageQueue of
		[] ->
			{ListToPrint,ListToQueue};
		[Name,Clock,Msg|T]->
			%% Here is the deal
			case check_clock(Name,Clock) of
				{N,safe}->
					find_safe_to_print(T,ListToPrint++[Name,Clock,Msg],ListToQueue);
				{N,unsafe}->
					find_safe_to_print(T,ListToPrint,ListToQueue++[Name,Clock,Msg])
			end
	end.

check_clock(Name,Clock)->
	{_,Number} = lists:keyfind(Name, 1, Clock),
	{Number,safe},
	{Number,unsafe}.


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
