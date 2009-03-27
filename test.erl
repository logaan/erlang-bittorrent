#!/usr/bin/env escript

main(_) ->
  make:files([bittorrent]),
  % Manhattan Murder Mystery
  MMM = bittorrent:start_peer('localhost', 51413, <<200, 23, 161, 167, 183, 43, 142, 38, 51, 130, 26, 222, 105, 118, 208, 38, 170, 97, 237, 59>>),
  % Caligula
  % Caligula = bittorrent:start_peer('localhost', 51413, <<254, 192, 154, 127, 239, 142, 10, 116, 16, 251, 160, 156, 240, 57, 35, 50, 60, 13, 15, 37>>),
  timer:sleep(infinity).

