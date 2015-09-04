%% @author Nikos
%% @doc @todo Add description to hist.

-module(hist).
-export([new/1,update/3]).

% Return a new history, where messages from Name will always be seen as old.
new(Name)-> [{Name, 0}].

% Check if message number N from the Node is old or new.
% If it is old then return old but if it new return {new, Updated}
% where Updated is the updated history.
update(Node, N, History)->
  case lists:keyfind(Node, 1, History) of
    {Name, Counter} ->
      if
        N > Counter ->
          {new, lists:keyreplace(Name, 1, History, {Name, N})};
        true -> old
      end;
    false ->
      {new, new(Node)  ++ History}
  end.