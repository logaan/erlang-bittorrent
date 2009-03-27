-module(bittorrent).
-export([start_peer/2]).
-compile(export_all).

start_peer(Host, Port) ->
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, false}]),
  ok = gen_tcp:send(Socket, list_to_binary([
    19, % Protocol string length
    "BitTorrent protocol", % Protocol string
    <<0,0,0,0,0,0,0,0>>, % Reseved space
    % Manhattan Murder Mystery (Woody Allen)
    <<200, 23, 161, 167, 183, 43, 142, 38, 51, 130, 26, 222, 105, 118, 208, 38, 170, 97, 237, 59>>, % Info Hash
    "-AZ4004-znmphhbrij37" % Peer ID
  ])),
  case gen_tcp:recv(Socket, 68) of
    {ok,Bin} ->
      <<
        19,
        "BitTorrent protocol",
        _Reserved:8/binary,
        InfoHash:20/binary,
        _PeerID:20/binary
      >> = Bin,
      spawn(?MODULE, peer_loop, [Socket, InfoHash]);
    Other ->
      io:format("Unexpected Match: ~p~n", [Other])
  end.

peer_loop(Socket, InfoHash) ->
  read_bitfield(Socket, InfoHash),
  inet:setopts(Socket,[{active,true}]),
  receive
    ping ->
      io:format("pong\n"),
      peer_loop(Socket, InfoHash);
    destroy ->
      gen_tcp:close(Socket),
      io:format(":(\n");
    Other ->
      io:format("Other: ~p~n", [Other]),
      peer_loop(Socket, InfoHash)
  end.

read_bitfield(Socket, _InfoHash) ->
  {ok,<<BinaryLength:4/binary, 5>>} = gen_tcp:recv(Socket, 5),
  Length = list_to_multibyte_integer(BinaryLength) - 1,
  {ok,Bin} = gen_tcp:recv(Socket, Length),
  io:format("Payload: ~p~n", [Bin]).

list_to_multibyte_integer(List) ->
  list_to_multibyte_integer(List, 0).
list_to_multibyte_integer([], Result) ->
  Result;
list_to_multibyte_integer([H|T], Result) ->
  NewResult = (Result bsl 8) + H,
  list_to_multibyte_integer(T, NewResult).

