#!/usr/bin/env escript

main(_) ->
  make:files([bittorrent]),
  % The Goonies
  _Pid = bittorrent:start_peer('localhost', 51413, <<215, 208, 134, 72, 230, 48, 54, 215, 96, 57, 20, 44, 91, 94, 115, 218, 87, 106, 30, 236>>),
  % Pid ! {socket, self()},
  % receive Socket -> ok end,
  timer:sleep(100000).
  % erlang:display("sending interest and request"),
  % bittorrent:send_interested(Socket),
  % bittorrent:send_request(Socket, 399, 1, 8),
  % receive Anything -> erlang:display(Anything) end.

