#!/usr/bin/env escript

main(_) ->
  make:files([bittorrent]),
  % Manhattan Murder Mystery (Woody Allen)
  Pid = bittorrent:start_peer('localhost', 51413, <<200, 23, 161, 167, 183, 43, 142, 38, 51, 130, 26, 222, 105, 118, 208, 38, 170, 97, 237, 59>>),
  timer:sleep(infinity).

