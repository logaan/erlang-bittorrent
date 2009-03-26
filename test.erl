#!/usr/bin/env escript

main(_) ->
  make:files([bittorrent]),
  Pid = bittorrent:start_peer('localhost', 51413),
  Pid ! ping,
  Pid ! ping,
  Pid ! destroy,
  Pid ! ping.

