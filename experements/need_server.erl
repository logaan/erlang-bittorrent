#!/usr/bin/env escript

main(_) ->
  Controller = self(),
  Pid = spawn_link(fun() -> need_server(Controller, 10) end),
  process_flag(trap_exit, true),
  Pid ! {set_status, 7, received},
  timer:sleep(2000),
  Pid ! done,
  regurgitate().

need_server(Controller, NumberOfPieces) ->
  Needs = ets:new(needs, []),
  populate(Needs, NumberOfPieces),
  loop(Controller, Needs).

loop(Controller, Needs) ->
  receive
    {set_status, PieceIndex, Status} ->
      io:format("Piece ~p ~p~n", [PieceIndex, Status]),
      ets:update_element(Needs, PieceIndex, {2, Status}),
      loop(Controller, Needs);
    done ->
      exit(normal);
    Unknown ->
      error_logger:error_msg(Unknown)
  after 1000 ->
      case length(ets:match(Needs, {'_', requested})) of
        0 ->
          {Availables, _Continuation} = ets:match(Needs, {'$1', available}, 5),
          FormattedAvailables = [ A || [A] <- Availables ],
          Controller ! {send_haves, FormattedAvailables};
        _ ->
          loop(Controller, Needs)
      end
  end.

populate(Needs, 0) ->
  Needs;
populate(Needs, NumberOfPieces) ->
  ets:insert(Needs, {NumberOfPieces, available}),
  populate(Needs, NumberOfPieces - 1).

regurgitate() ->
  receive
    Anything ->
      erlang:display(Anything),
      regurgitate()
  after 5000 ->
      ok
  end.
