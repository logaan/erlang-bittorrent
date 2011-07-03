#!/usr/bin/env escript

main(_) ->
  {ok, File} = file:open("gpl.txt", [read]),
  {ok, Data} = file:pread(File, 0, 2379),
  ok = file:close(File),
  erlang:display(Data).

