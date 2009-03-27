-module(bittorrent).
-export([start_peer/3]).
-compile(export_all).

start_peer(Host, Port, InfoHash) ->
  spawn(?MODULE, start_loop, [Host, Port, InfoHash]).

start_loop(Host, Port, InfoHash) ->
  io:format("Starting loop for ~p~n", [InfoHash]),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}]),
  {ok, _PeerID} = handshake(Socket, InfoHash),
  loop(Socket).

loop(Socket) ->
  receive
    {tcp,Socket,<<0,0,0,0,0,0,0,0>>} ->
      io:format("Got a keepalive~n"),
      send_keepalive(Socket),
      bittorrent:loop(Socket);

    {tcp,Socket,<<0,0,0,1,1>>} ->
      io:format("You are unchoked~n"),
      bittorrent:loop(Socket);

    {tcp,Socket,<<MessageLength:4/binary, MessageID/integer, Tail/binary>>} ->
      handle_message(list_to_binary([MessageLength, MessageID, Tail])),
      bittorrent:loop(Socket);

    {tcp_closed,Socket} ->
      io:format("Connection was closed~n");

    Anything ->
      io:format("Got ~p~n", [Anything]),
      bittorrent:loop(Socket)

  end.

handle_message(Message) ->
  case Message of
    <<MessageLength:4/binary, 5, Tail/binary>> ->
      Length = binary_to_multibyte_integer(MessageLength),
      io:format("Received bitfield of length ~p~n", [Length])
  end.

handshake(Socket, InfoHash) ->
  gen_tcp:send(Socket, list_to_binary([
    19,                    % Protocol string length
    "BitTorrent protocol", % Protocol string
    <<0,0,0,0,0,0,0,0>>,   % Reseved space
    InfoHash,              % Info Hash
    "-AZ4004-znmphhbrij37" % Peer ID
  ])),
  receive
    {tcp,Socket,<<
        19,
        "BitTorrent protocol",
        _Reserved:8/binary,
        _InfoHash:20/binary,
        PeerID:20/binary
      >>} ->
      io:format("Received handshake from ~p~n", [PeerID]),
      {ok, PeerID}
  end.

send_keepalive(Socket) ->
  gen_tcp:send(Socket, <<0,0,0,0,0,0,0,0>>).

binary_to_multibyte_integer(Binary) ->
  List = binary_to_list(Binary),
  list_to_multibyte_integer(List, 0).
list_to_multibyte_integer([], Result) ->
  Result;
list_to_multibyte_integer([H|T], Result) ->
  NewResult = (Result bsl 8) + H,
  list_to_multibyte_integer(T, NewResult).

