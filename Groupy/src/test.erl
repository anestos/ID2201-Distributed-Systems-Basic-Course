-module(test).
-export([start/2]).

start(Module, Number) ->
	Leader = worker:start(1, Module, 100, 10000),
	start_slaves(Leader, Module, Number),
	Leader.

start_slaves(Leader,Module,Number) ->
	case Number > 0 of 
		true ->	
			register(list_to_atom("worker" ++ integer_to_list( Number+1 )), spawn(fun() -> worker:start(Number+1, Module, 99*Number,Leader, 3000*Number) end)),		
			start_slaves(Leader, Module, Number-1);
		false->
			false
	end.