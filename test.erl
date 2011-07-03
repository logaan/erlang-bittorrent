#!/usr/bin/env escript

main(_) ->
  make:files([bittorrent,multibyte, bencode, sha1, meta_info]),
  MetaInfo = meta_info:read_file("gpl.txt.torrent"),
  InfoHash = meta_info:info_hash(MetaInfo),
  _Pid = bittorrent:start_peer(self(), 'localhost', 51413, InfoHash),

  receive {socket, Socket} -> ok end,

  bittorrent:send_bitfield(Socket, [3]),

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
  FileName = meta_info:filename(MetaInfo),

  IntegerBlockOffset = multibyte:binary_to_multibyte_integer(BlockOffset) +
                       multibyte:binary_to_multibyte_integer(PieceIndex) * PieceLength,
  IntegerBlockLength = multibyte:binary_to_multibyte_integer(BlockLength),
  {ok, File} = file:open(FileName, [read]),
  {ok, Data} = file:pread(File, IntegerBlockOffset, IntegerBlockLength),
  ok = file:close(File),
  bittorrent:send_piece(Socket, PieceIndex, BlockOffset, Data).
