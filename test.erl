#!/usr/bin/env escript

-define(DATA_DIRECTORY, "data/").

main(_) ->
  code:add_path("ebin"),
  % MetaInfo = meta_info:read_file("data/gpl.txt.torrent"),
  MetaInfo = meta_info:read_file("data/Hack the Planet.png.torrent"),
  InfoHash = meta_info:info_hash(MetaInfo),
  _Pid = bittorrent:start_peer(self(), 'localhost', 51413, InfoHash),

  receive {socket, Socket} -> ok end,

  NeedServer = need_server:start(meta_info:number_of_pieces(MetaInfo)),

  bittorrent:send_bitfield(Socket, meta_info:bitfield(MetaInfo)),

  % Needs to be unchocked before it'll start requesting pieces
  bittorrent:send_unchoke(Socket),

  loop(MetaInfo, Socket, NeedServer).

loop(MetaInfo, Socket, NeedServer) ->
  receive
    {received_requst, PieceIndex, BlockOffset, BlockLength} ->
      % Terrible name
      PieceNumber = multibyte:binary_to_multibyte_integer(PieceIndex),
      need_server:set_requested(NeedServer, PieceNumber),
      send_any_piece(Socket, PieceIndex, BlockOffset, BlockLength, MetaInfo),
      need_server:set_sent(NeedServer, PieceNumber),
      loop(MetaInfo, Socket, NeedServer);
    {have, PieceIndex} ->
      need_server:set_received(NeedServer, PieceIndex),
      loop(MetaInfo, Socket, NeedServer);
    {send_haves, Haves} ->
      io:format("Should send haves: ~p~n", [Haves]),
      SetAndSend = fun(PieceIndex) ->
        need_server:set_offered(NeedServer, PieceIndex),
        bittorrent:send_have(Socket, PieceIndex)
      end,
      lists:map(SetAndSend, Haves),
      loop(MetaInfo, Socket, NeedServer);
    closed ->
      need_server:stop(NeedServer),
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
