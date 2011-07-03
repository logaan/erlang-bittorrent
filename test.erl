#!/usr/bin/env escript

-define(DATA_DIRECTORY, "data/").

main(_) ->
  code:add_path("ebin"),
  % MetaInfo = meta_info:read_file("data/gpl.txt.torrent"),
  MetaInfo = meta_info:read_file("data/Hack the Planet.png.torrent"),
  InfoHash = meta_info:info_hash(MetaInfo),
  _Pid = bittorrent:start_peer(self(), 'localhost', 51413, InfoHash),

  receive {socket, Socket} -> ok end,

  % receive
  %   unchoked -> ok
  %   after 5000 -> ok
  % end,

  bittorrent:send_bitfield(Socket, meta_info:bitfield(MetaInfo)),

  % Need to send some haves to start getting requests
  bittorrent:send_have(Socket, 0),
  bittorrent:send_have(Socket, 1),

  % Needs to be unchocked before it'll start requesting pieces
  bittorrent:send_unchoke(Socket),

  loop(MetaInfo, Socket).

loop(MetaInfo, Socket) ->
  receive
    {received_requst, PieceIndex, BlockOffset, BlockLength} ->
      send_any_piece(Socket, PieceIndex, BlockOffset, BlockLength, MetaInfo),
      loop(MetaInfo, Socket);
    closed ->
      ok;
    received_bitfield ->
      io:format("Transmission already has the complete torrent.");
    Unknown ->
      io:format("Received unknown message:"),
      erlang:display(Unknown)
  end.

send_any_piece(Socket, PieceIndex, BlockOffset, BlockLength, MetaInfo) ->
  PieceLength = meta_info:piece_length(MetaInfo),
  FileName = filename:join(?DATA_DIRECTORY, meta_info:filename(MetaInfo)),

  IntegerBlockOffset = multibyte:binary_to_multibyte_integer(BlockOffset) +
                       multibyte:binary_to_multibyte_integer(PieceIndex) * PieceLength,
  IntegerBlockLength = multibyte:binary_to_multibyte_integer(BlockLength),
  {ok, File} = file:open(FileName, [read]),
  {ok, Data} = file:pread(File, IntegerBlockOffset, IntegerBlockLength),
  ok = file:close(File),
  bittorrent:send_piece(Socket, PieceIndex, BlockOffset, Data).
