#!/usr/bin/env escript

% Looks like currently it's not taking the piece number into account when
% calculating the offset of the data to send. Need to multiply the Piece Index
% by Piece Length and then add Offset. Which will mean we need to pull Piece
% Length out of the meta info file.

read_info_hash(FileName) ->
  {ok, FileContents} = file:read_file(FileName),
  {{dict, MetaInfoDict}, _Remainder} = bencode:decode(FileContents),
  Info = dict:fetch(<<"info">>, MetaInfoDict),
  BencodedInfo = binary_to_list(bencode:encode(Info)),
  sha1:binstring(BencodedInfo).

main(_) ->
  make:files([bittorrent,multibyte, bencode, sha1]),
  InfoHash = read_info_hash("gpl.txt.torrent"),
  % GPL 3.0
  _Pid = bittorrent:start_peer(self(), 'localhost', 51413, InfoHash),

  receive {socket, Socket} -> ok end,
  bittorrent:send_bitfield(Socket, [2]),
  bittorrent:send_have(Socket, 0),
  bittorrent:send_have(Socket, 1),

  loop(Socket).


loop(Socket) ->
  bittorrent:send_choke(Socket),
  bittorrent:send_unchoke(Socket),
  receive
    interested ->
      erlang:display("woot interested");
    {received_requst, PieceIndex, BlockOffset, BlockLength} ->
      send_any_piece(Socket, PieceIndex, BlockOffset, BlockLength);
    received_bitfield ->
      erlang:display("Sent bitfield. Test probs won't work.");
    AnythingAgain ->
      erlang:display("AnythingAgain"),
      erlang:display(AnythingAgain)
    after 6000 -> ok
  end,

  loop(Socket).

send_any_piece(Socket, PieceIndex, BlockOffset, BlockLength) ->
  % 32768 is the piece length of this particular torrent file
  IntegerBlockOffset = multibyte:binary_to_multibyte_integer(BlockOffset) +
                       multibyte:binary_to_multibyte_integer(PieceIndex) * 32768,
  IntegerBlockLength = multibyte:binary_to_multibyte_integer(BlockLength),
  {ok, File} = file:open("gpl.txt", [read]),
  {ok, Data} = file:pread(File, IntegerBlockOffset, IntegerBlockLength),
  ok = file:close(File),
  bittorrent:send_piece(Socket, PieceIndex, BlockOffset, Data).
