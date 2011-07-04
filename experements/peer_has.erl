#!/usr/bin/env escript

main(_) ->
  need_server(10).

need_server(NumberOfPieces) ->
  % create an set type ets
  Needs = ets:new(needs, []),
  populate(Needs, NumberOfPieces),
  ets:update_element(Needs, 2, {2, recieved}),

  erlang:display(ets:member(Needs, 1)),
  erlang:display(ets:member(Needs, 3)),
  erlang:display(ets:match(Needs, {'$1', available})),
  erlang:display(ets:match(Needs, {'$1', recieved})).

populate(Needs, 0) ->
  Needs;
populate(Needs, NumberOfPieces) ->
  ets:insert(Needs, {NumberOfPieces, available}),
  populate(Needs, NumberOfPieces - 1).
