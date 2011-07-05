-module(meta_info).
-include_lib("eunit/include/eunit.hrl").
-export([read_file/1, info_hash/1, filename/1, piece_length/1,
         number_of_pieces/1, bitfield/1]).

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

number_of_pieces({meta_info, MetaInfo}) ->
  PieceHashes = lookup_info_field(<<"pieces">>, MetaInfo),
  HashLength = 20,
  round(length(binary_to_list(PieceHashes)) / HashLength).

bitfield({meta_info, MetaInfo}) ->
  NumberOfPieces = number_of_pieces({meta_info, MetaInfo}),
  create_bitfield_binary(NumberOfPieces).

%
% Internal
%
lookup_info_field(Name, MetaInfo) ->
  {dict, Info} = dict:fetch(<<"info">>, MetaInfo),
  dict:fetch(Name, Info).

create_bitfield_binary(0) ->
  <<>>;
create_bitfield_binary(NumberOfPieces) ->
  % Here be dragons. By doing NumberOfPieces rem 8 we get the number of bits
  % that won't be fitting nicely into a byte. So they need to be padded out
  % with leading 0s. To find out how many padding zeros we need we subtract
  % that number from 8. But if we had no overflow originally subtracting it
  % from 8 will give us 8. The final rem 8 turns that potential 8 into a 0
  % while leaving any other values untouched.
  LeaderLength = (8 - NumberOfPieces rem 8) rem 8,
  create_bitfield_binary(<<>>, LeaderLength, NumberOfPieces).

create_bitfield_binary(Binary, 0, 0) ->
  Binary;
create_bitfield_binary(Binary, 0, NumberOfPieces) ->
  NewBinary = <<Binary/bitstring, 1:1>>,
  create_bitfield_binary(NewBinary, 0, NumberOfPieces - 1);
create_bitfield_binary(Binary, LeaderLength, NumberOfPieces) ->
  NewBinary = <<Binary/bitstring, 0:1>>,
  create_bitfield_binary(NewBinary, LeaderLength - 1, NumberOfPieces).

%
% Tests
%

bitfield_binary_0_test() ->
  <<>> = create_bitfield_binary(0).
bitfield_binary_1_test() ->
  <<1>> = create_bitfield_binary(1).
bitfield_binary_2_test() ->
  <<3>> = create_bitfield_binary(2).
bitfield_binary_3_test() ->
  <<7>> = create_bitfield_binary(3).
bitfield_binary_4_test() ->
  <<15>> = create_bitfield_binary(4).
bitfield_binary_5_test() ->
  <<31>> = create_bitfield_binary(5).
bitfield_binary_6_test() ->
  <<63>> = create_bitfield_binary(6).
bitfield_binary_7_test() ->
  <<127>> = create_bitfield_binary(7).
bitfield_binary_8_test() ->
  <<255>> = create_bitfield_binary(8).

concept_test() ->
  A = <<>>,                
  B = <<A/bitstring, 0:1>>,
  C = <<B/bitstring, 0:1>>,
  D = <<C/bitstring, 1:1>>,
  E = <<D/bitstring, 1:1>>,
  F = <<E/bitstring, 1:1>>,
  G = <<F/bitstring, 1:1>>,
  H = <<G/bitstring, 1:1>>,
  I = <<H/bitstring, 1:1>>,
  <<63>> = I.
