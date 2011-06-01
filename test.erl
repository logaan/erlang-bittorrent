#!/usr/bin/env escript

main(_) ->
  make:files([bittorrent,multibyte]),
  % GPL 3.0
  _Pid = bittorrent:start_peer(self(), 'localhost', 51413, <<10, 95, 101, 190, 89, 72, 197, 106, 133, 8, 130, 209, 137, 208, 62, 17, 221, 73, 18, 249>>),

  receive {socket, Socket} -> ok end,
  bittorrent:send_bitfield(Socket, [2]),
  bittorrent:send_have(Socket, 1),

  % Not sure what this was here for. Perhaps to manually nudge the
  % process along?
  % receive received_bitfield -> ok end,
  bittorrent:send_choke(Socket),

  receive
    Anything -> erlang:display(Anything)
  after 6000 -> ok
  end,
  bittorrent:send_unchoke(Socket),

  receive AnythingAgain -> erlang:display(AnythingAgain) end.

