#!/usr/bin/env escript

main(_) ->
  make:files([bittorrent]),
  % Kubuntu
  Pid = bittorrent:start_peer('localhost', 51413, <<109, 186, 5, 143, 40, 41, 235, 158, 243, 14, 8, 241, 161, 229, 214, 33, 41, 8, 161, 93>>),
  Pid ! {socket, self()},
  receive Socket -> ok end,
  timer:sleep(10000),
  erlang:display("sending interest and request"),
  bittorrent:send_interested(Socket),
  bittorrent:send_request(Socket, 399, 1, 8),
  receive Anything -> erlang:display(Anything) end.

