#!/usr/bin/env escript

main(_) ->
  make:files([bittorrent,multibyte, bencode, sha1, meta_info]),
  MetaInfo = meta_info:read_file("Hack the Planet.png.torrent"),
  InfoHash = meta_info:info_hash(MetaInfo),
  _Pid = bittorrent:start_peer(self(), 'localhost', 51413, InfoHash),

  receive {socket, Socket} -> ok end,
  bittorrent:send_bitfield(Socket, [2]),
  bittorrent:send_have(Socket, 0),
  bittorrent:send_have(Socket, 1),

  loop(MetaInfo, Socket).

loop(MetaInfo, Socket) ->
  bittorrent:send_choke(Socket),
  bittorrent:send_unchoke(Socket),

  receive
    interested ->
      erlang:display("woot interested");
    {received_requst, PieceIndex, BlockOffset, BlockLength} ->
      send_any_piece(Socket, PieceIndex, BlockOffset, BlockLength, MetaInfo);
    received_bitfield ->
      erlang:display("Sent bitfield. Test probs won't work.");
    AnythingAgain ->
      erlang:display("AnythingAgain"),
      erlang:display(AnythingAgain)
    after 6000 -> ok
  end,

  loop(MetaInfo, Socket).

send_any_piece(Socket, PieceIndex, BlockOffset, BlockLength, MetaInfo) ->
  erlang:display("got her"),
  PieceLength = meta_info:piece_length(MetaInfo),
  FileName = meta_info:filename(MetaInfo),
  erlang:display(FileName),

  IntegerBlockOffset = multibyte:binary_to_multibyte_integer(BlockOffset) +
                       multibyte:binary_to_multibyte_integer(PieceIndex) * PieceLength,
  IntegerBlockLength = multibyte:binary_to_multibyte_integer(BlockLength),
  {ok, File} = file:open(FileName, [read]),
  {ok, Data} = file:pread(File, IntegerBlockOffset, IntegerBlockLength),
  ok = file:close(File),
  bittorrent:send_piece(Socket, PieceIndex, BlockOffset, Data).
