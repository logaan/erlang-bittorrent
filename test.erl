#!/usr/bin/env escript

main(_) ->
  make:files([bittorrent]),
  Pid = bittorrent:start_peer('localhost', 51413),
  Pid ! i_love_you,
  Pid ! i_love_you,
  Pid ! kill_yourself,
  Pid ! i_love_you.

