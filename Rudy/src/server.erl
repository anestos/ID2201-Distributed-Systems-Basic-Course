%% cd('/Users/Nikos/Desktop/Erlang/Rudy/src').
%% {rudy, 'lorenzo@130.229.164.164'} ! test:bench('130.229.164.164', 8080).
-module(server).
-export([start/1, stop/0]).
%%-import('http',[parse_request/1]).      http:->

init(Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      handler(Listen),
      gen_tcp:close(Listen);
    {error, Error} ->
      error
  end.

handler(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      request(Client),
      handler(Listen);
    {error, Error} ->
      error
  end.

request(Client) ->
  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} ->

      Request = http:parse_request(Str),
      Response = reply(Request),
      gen_tcp:send(Client, Response);
    {error, Error} ->
      io:format("rudy: error: ~w~n", [Error])
  end,
  gen_tcp:close(Client).
reply({{get, URI, _}, _, _}) ->
  % {ok, Content} = file:read_file(string:substr(URI, 2) ++ "/index.html"),
  % http:ok(Content).
  % Fun = string:substr(URI, 2),
  % {ok, File} = file:read_file(Fun ++ "/index.html"),
  % Content = unicode:characters_to_list(File),
  http:ok(URI).


start(Port) ->
  register(rudy, spawn(fun() -> init(Port) end)).
stop() ->
  exit(whereis(rudy), "time to die").
