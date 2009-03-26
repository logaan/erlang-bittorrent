-module(bittorrent).
-export([start_peer/2]).
-compile(export_all).

start_peer(Host, Port) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false}]),
  ok = gen_tcp:send(Socket, list_to_binary([
    19, % Protocol string length
    "BitTorrent protocol", % Protocol string
    <<0,0,0,0,0,0,0,0>>, % Reseved space
    % Manhattan Murder Mystery (Woody Allen)
    <<200, 23, 161, 167, 183, 43, 142, 38, 51, 130, 26, 222, 105, 118, 208, 38,
      170, 97, 237, 59>>, % Info Hash
    "-AZ4004-znmphhbrij37" % Peer ID
  ])),
  case gen_tcp:recv(Socket, 68) of
    {ok,Bin} ->
      <<
        19,
        "BitTorrent protocol",
        _Reserved:8/binary,
        InfoHash:20/binary,
        _PeerID:20/binary
      >> = Bin,
      spawn(?MODULE, peer_loop, [Socket, InfoHash]);
    {error,closed} ->
      io:format("The connection has been closed. Perhaps you already have a connection open.~n");
    Other ->
      io:format("Weird Match: ~p~n", [Other])
  end.

peer_loop(Socket, InfoHash) ->
  {ok,Bin} = gen_tcp:recv(Socket, 0),
  io:format("Tail?: ~p~n", [Bin]),
  inet:setopts(Socket,[{active,true}]),
  receive
    ping ->
      io:format("pong\n"),
      peer_loop(Socket, InfoHash);
    destroy ->
      gen_tcp:close(Socket),
      io:format(":(\n");
    Other ->
      io:format("Other: ~p~n", [Other]),
      peer_loop(Socket, InfoHash)
  end.

