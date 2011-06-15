#!/usr/bin/env escript

main(_) ->
  make:files([bittorrent,multibyte]),
  % GPL 3.0
  _Pid = bittorrent:start_peer(self(), 'localhost', 51413, <<157, 149, 198, 12, 174, 192, 204, 147, 208, 94, 100, 68, 57, 70, 142, 103, 45, 195, 184, 120>>),

  receive {socket, Socket} -> ok end,
  bittorrent:send_bitfield(Socket, [2]),
  bittorrent:send_have(Socket, 1),

  % Not sure what this was here for. Perhaps to manually nudge the
  % process along?
  % receive received_bitfield -> ok end,
  bittorrent:send_choke(Socket),

  loop(Socket).


loop(Socket) ->
  receive
    interested -> erlang:display("woot interested");
    Anything ->
      erlang:display("AnythingAgain"),
      erlang:display(Anything)
  after 6000 -> ok
  end,
  bittorrent:send_unchoke(Socket),
  bittorrent:send_have(Socket, 0),

  receive
    {received_requst, PieceIndex, BlockOffset, BlockLength} ->
      send_any_piece(Socket, PieceIndex, BlockOffset, BlockLength),
      bittorrent:send_choke(Socket);
    AnythingAgain ->
      erlang:display("AnythingAgain"),
      erlang:display(AnythingAgain)
  end,
  
  loop(Socket).

send_any_piece(Socket, PieceIndex, BlockOffset, BlockLength) ->
  IntegerBlockOffset = multibyte:binary_to_multibyte_integer(BlockOffset),
  IntegerBlockLength = multibyte:binary_to_multibyte_integer(BlockLength),
  {ok, File} = file:open("gpl.txt", [read]),
  {ok, Data} = file:pread(File, IntegerBlockOffset, IntegerBlockLength),
  ok = file:close(File),
  bittorrent:send_piece(Socket, PieceIndex, BlockOffset, Data).
