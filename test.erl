#!/usr/bin/env escript

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
