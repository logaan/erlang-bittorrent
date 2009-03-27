-module(bittorrent).
-export([start_peer/3]).
-compile(export_all).

% Spawns a new peer process and returns the pid.
start_peer(Host, Port, InfoHash) ->
  spawn(?MODULE, start_loop, [Host, Port, InfoHash]).


% Creates the connection to the peer and handshakes before handing off to the
% main loop.
start_loop(Host, Port, InfoHash) ->
  io:format("Starting loop for ~p~n", [InfoHash]),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}]),
  {ok, _PeerID} = handshake(Socket, InfoHash),
  loop(Socket).


% This loop handles keepalives, closed connections and passes on messages to
% their own methods.
loop(Socket) ->
  receive
    {tcp,Socket,<<0,0,0,0,0,0,0,0>>} ->
      io:format("Got a keepalive~n"),
      send_keepalive(Socket),
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


% Each message has it's own method. Any data tacked onto the end of a message is
% passed back into this method until there is no more data, which returns ok.
handle_message(<<>>) ->
  ok;

handle_message(<<0,0,0,1,1, Tail/binary>>) ->
  io:format("You are unchoked~n"),
  handle_message(Tail);

handle_message(<<MessageLength:4/binary, 5, Tail/binary>>) ->
  Length  = binary_to_multibyte_integer(MessageLength) - 1,
  <<Payload:Length/binary, UnusedTail/binary>> = Tail,
  io:format("Received bitfield of length ~p with payload ~n~p~n", [Length, Payload]),
  handle_message(UnusedTail);

handle_message(Other) ->
  io:format("Got a weird message: ~p~n", [Other]).


% Sends and receives the initial handshake message
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


% Sends a message to keep the connection open.
send_keepalive(Socket) ->
  gen_tcp:send(Socket, <<0,0,0,0,0,0,0,0>>).


% Will convert the binary into an integer by bitshifting each byte left.
binary_to_multibyte_integer(Binary) ->
  List = binary_to_list(Binary),
  list_to_multibyte_integer(List, 0).

list_to_multibyte_integer([], Result) ->
  Result;

list_to_multibyte_integer([H|T], Result) ->
  NewResult = (Result bsl 8) + H,
  list_to_multibyte_integer(T, NewResult).

