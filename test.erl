#!/usr/bin/env escript

main(_) ->
  make:files([bittorrent,multibyte]),
  % GPL 3.0
  Pid = bittorrent:start_peer(self(), 'localhost', 51413, <<10, 95, 101, 190, 89, 72, 197, 106, 133, 8, 130, 209, 137, 208, 62, 17, 221, 73, 18, 249>>),
  erlang:display(Pid),

  receive {socket, Socket} -> ok end,
  bittorrent:send_bitfield(Socket, [2]),
  bittorrent:send_have(Socket, 1),

  receive received_bitfield -> ok end,
  bittorrent:send_choke(Socket),

  receive
    Anything -> erlang:display(Anything)
  after 6000 -> ok
  end,
  bittorrent:send_unchoke(Socket),

  % timer:sleep(100000).
  % erlang:display("sending interest and request"),
  % bittorrent:send_interested(Socket),
  % bittorrent:send_request(Socket, 399, 1, 8),
  receive AnythingAgain -> erlang:display(AnythingAgain) end.

