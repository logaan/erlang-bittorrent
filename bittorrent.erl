-module(bittorrent).
-export([start_peer/2]).
-compile(export_all).

start_peer(Host, Port) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
  ok = gen_tcp:send(Socket, list_to_binary([
    19, % Protocol string length
    "BitTorrent protocol", % Protocol string
    <<0,0,0,0,0,0,0,0>>, % Reseved space
    <<60, 12, 28, 181, 105, 136, 94, 238, 188, 226, 221, 253, 177, 52, 40, 78, 111, 80, 4, 172>>, % Info Hash
    "-AZ4004-znmphhbrij37" % Peer ID
  ])),
  receive
    {tcp,Socket,Bin} ->
      <<
        _PstrLen:1/binary,
        _Pstr:19/binary,
        _Reserved:8/binary,
        InfoHash:20/binary,
        _PeerID:20/binary
      >> = Bin,
      spawn(fun() -> peer_loop(Socket, InfoHash) end)
  end.

peer_loop(Socket, InfoHash) ->
  receive
    i_love_you ->
      io:format("keep it coming baby\n"),
      peer_loop(Socket, InfoHash);
    kill_yourself ->
      io:format("i hate you so much\n")
  end.

