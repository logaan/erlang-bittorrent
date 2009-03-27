-module(bittorrent).
-export([start_peer/2]).
-compile(export_all).

start_peer(Host, Port) ->
  spawn(?MODULE, start_loop, [Host, Port]).

start_loop(Host, Port) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}]),
  ok = gen_tcp:send(Socket, list_to_binary([
    19, % Protocol string length
    "BitTorrent protocol", % Protocol string
    <<0,0,0,0,0,0,0,0>>, % Reseved space
    % Manhattan Murder Mystery (Woody Allen)
    <<200, 23, 161, 167, 183, 43, 142, 38, 51, 130, 26, 222, 105, 118, 208, 38, 170, 97, 237, 59>>, % Info Hash
    "-AZ4004-znmphhbrij37" % Peer ID
  ])),
  loop().

loop() ->
  receive
    Anything ->
      io:format("Got ~p~n", [Anything]),
      loop()
  end.
