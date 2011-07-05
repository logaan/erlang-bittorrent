#!/usr/bin/env escript

main(_) ->
  make:files(["../src/need_server.erl"]),
  Pid = need_server:start(10),
  need_server:set_received(Pid, 7),
  timer:sleep(2000),
  need_server:stop(Pid),
  regurgitate().

% Utilities
regurgitate() ->
  receive
    Anything ->
      erlang:display(Anything),
      regurgitate()
  after 5000 ->
      ok
  end.
