-module(storage).
-compile(export_all).

create()->[].

add(Store,Key,Value)->
	[{Key,Value}|Store].

lookup(Store,Key)->
	lists:keyfind(Key, 1, Store).

split(Key,Store)->
	Sorted = lists:keysort(1, Store),
	split(Sorted,Key,[]).

split(Store,Key,Give)->
	case Store of 
		[]->
			{[],[]};
		[{SomeKey,SomeValue}|T]->
			case Key<SomeKey of
				true->
					{Store,Give};
				false->			
					split(T,Key,[{SomeKey,SomeValue}]++Give)
			end
	end.

merge(StoreA, StoreB)->
	lists:append(StoreA,StoreB).