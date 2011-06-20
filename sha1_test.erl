#!/usr/bin/env escript

% The handshake requires a info hash. It's a 20 byte sha1 of the info key in
% the meta info file. The info key is the dictionary of all of the torrent
% files.

% Hear we decode the meta info file to find the info entry. We then bencode the
% info entry and take the first 20 bytes of it's sha1.
main(_) ->
  make:files([bencode, sha1]),
  {ok, File} = file:read_file("gpl.txt.torrent"),
  {{dict, MetaInfoDict}, _Remainder} = bencode:decode(File),
  Info = dict:fetch(<<"info">>, MetaInfoDict),
  BencodedInfo = binary_to_list(bencode:encode(Info)),
  InfoHash = sha1:binstring(BencodedInfo),
  io:format("~p~n", [InfoHash]).

