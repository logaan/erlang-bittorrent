-module(meta_info).
-export([read_file/1, info_hash/1, filename/1, piece_length/1]).

%
% Constructor
%
read_file(FileName) ->
  {ok, FileContents} = file:read_file(FileName),
  {{dict, MetaInfo}, _Remainder} = bencode:decode(FileContents),
  {meta_info, MetaInfo}.


%
% Accessors
%
info_hash({meta_info, MetaInfo}) ->
  Info = dict:fetch(<<"info">>, MetaInfo),
  BencodedInfo = binary_to_list(bencode:encode(Info)),
  sha1:binstring(BencodedInfo).

filename({meta_info, MetaInfo}) ->
  Name = lookup_info_field(<<"name">>, MetaInfo),
  binary_to_list(Name).

piece_length({meta_info, MetaInfo}) ->
  lookup_info_field(<<"piece length">>, MetaInfo).

%
% Internal
%
lookup_info_field(Name, MetaInfo) ->
  {dict, Info} = dict:fetch(<<"info">>, MetaInfo),
  dict:fetch(Name, Info).
