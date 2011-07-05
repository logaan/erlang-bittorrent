-module(need_server).
-export([start/1, stop/1, set_received/2, set_offered/2, set_requested/2,
         set_sent/2]).
% API
start(NumberOfPieces) ->
  Controller = self(),
  spawn_link(fun() -> need_server(Controller, NumberOfPieces) end).

stop(Pid) ->
  Pid ! stop.

set_received(Pid, PieceIndex) ->
  Pid ! {set_status, PieceIndex, received}.

set_offered(Pid, PieceIndex) ->
  Pid ! {set_status, PieceIndex, offered}.

set_requested(Pid, PieceIndex) ->
  Pid ! {set_status, PieceIndex, requested}.

set_sent(Pid, PieceIndex) ->
  Pid ! {set_status, PieceIndex, sent}.

% Server
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
    stop ->
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
