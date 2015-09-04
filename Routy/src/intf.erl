-module(intf).
-export([new/0,add/4,remove/2,lookup/2,ref/2,name/2,list/1,broadcast/2]).

new()-> [].

add(Name, Ref, Pid, Intf) -> 
	[{Name, Ref, Pid} | Intf].

remove(Name, Intf) ->
	lists:keydelete(Name, 1, Intf).

lookup(Name, Intf)->
	case lists:keyfind(Name,1,Intf) of
		{Name,_,Pid} ->
			{ok, Pid};
		false ->
			notfound
	end.

ref(Name, Intf)->
	case lists:keyfind(Name,1,Intf) of
		{Name,Ref,_} ->
			{ok, Ref};
		false ->
			notfound
	end.

name(Ref, Intf) -> 
	case lists:keyfind(Ref,2,Intf) of
		{Name,Ref,_} ->
			{ok, Name};
		false ->
			notfound
	end.

list(Intf) ->
	list(Intf, []).
list(Intf, NameList) ->
	case Intf of
		[] ->
			NameList;
		[{Name, _, _} | T] -> list(T, [Name | NameList])
	end.

broadcast(Message, Intf)->
	case Intf of 
		[]->
			ok;
		[{_, _, Pid}|T] ->
			Pid ! Message,
			io:format("Broadcasted: ~w ! ~w~n",[Pid,Message]),
			broadcast(Message,T)
	end.


