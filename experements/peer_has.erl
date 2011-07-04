#!/usr/bin/env escript

main(_) ->
  Pid = spawn_link(fun() -> need_server(10) end),
  process_flag(trap_exit, true),
  Pid ! {set_status, 7, received},
  timer:sleep(2000),
  Pid ! done,
  receive Anything -> erlang:display(Anything) end.

need_server(NumberOfPieces) ->
  % create an set type ets
  Needs = ets:new(needs, []),
  populate(Needs, NumberOfPieces),
  ets:update_element(Needs, 2, {2, received}),

  erlang:display(ets:member(Needs, 1)),
  erlang:display(ets:member(Needs, 3)),
  erlang:display(ets:match(Needs, {'$1', available})),
  erlang:display(ets:match(Needs, {'$1', received})),
  loop(Needs).

loop(Needs) ->
  receive
    {set_status, PieceIndex, Status} ->
      io:format("Piece ~p ~p~n", [PieceIndex, Status]),
      ets:update_element(Needs, PieceIndex, {2, Status}),
      loop(Needs);
    done ->
      exit(normal);
    Unknown ->
      error_logger:error_msg(Unknown)
  after 1000 ->
      case length(ets:match(Needs, {'_', requested})) of
        0 ->
          {Availables, _} = ets:match(Needs, {'$1', available}, 5),
          io:format("Send out some haves for ~p~n", [Availables]);
        _ ->
          loop(Needs)
      end
  end.

populate(Needs, 0) ->
  Needs;
populate(Needs, NumberOfPieces) ->
  ets:insert(Needs, {NumberOfPieces, available}),
  populate(Needs, NumberOfPieces - 1).
