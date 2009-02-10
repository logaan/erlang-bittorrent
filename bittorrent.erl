-module(bittorrent).
-export([handshake/2]).

handshake(Host, Port) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}]),
  ok = gen_tcp:send(Socket, <<19,
                              "BitTorrent protocol",
                              0,0,0,0,0,0,0,0,
                              55,17,201,153,197,215,145,33,109,167,182,47,182,199,5,250,166,103,45,134,
                              "-AZ4004-znmphhbrij37">>),
  receive_handshake(Socket).

receive_handshake(Socket) ->
  receive
    {tcp,Socket,Bin} ->
      <<PstrLen:1/binary, Pstr:19/binary, Reserved:8/binary, InfoHash:20/binary, PeerID:20/binary>> = Bin,
      {PstrLen, Pstr, Reserved, InfoHash, PeerID}
  end.

