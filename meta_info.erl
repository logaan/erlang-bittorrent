-module(meta_info).
-export([read_file/1, info_hash/1, piece_length/1]).

read_file(FileName) ->
  {ok, FileContents} = file:read_file(FileName),
  {{dict, MetaInfo}, _Remainder} = bencode:decode(FileContents),
  {meta_info, MetaInfo}.

info_hash({meta_info, MetaInfo}) ->
  Info = dict:fetch(<<"info">>, MetaInfo),
  BencodedInfo = binary_to_list(bencode:encode(Info)),
  sha1:binstring(BencodedInfo).

piece_length({meta_info, MetaInfo}) ->
  {dict, Info} = dict:fetch(<<"info">>, MetaInfo),
  PieceLength = dict:fetch(<<"piece length">>, Info),
  PieceLength.

