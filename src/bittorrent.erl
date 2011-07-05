-module(bittorrent).
-export([start_peer/4]).
-compile(export_all).

% Spawns a new peer process and returns the pid.
start_peer(Master, Host, Port, InfoHash) ->
  register(master, Master),
  spawn_link(?MODULE, start_loop, [Host, Port, InfoHash]).


% Creates the connection to the peer and handshakes before handing off to the
% main loop.
start_loop(Host, Port, InfoHash) ->
  io:format("Starting loop for ~p~n", [InfoHash]),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}]),
  {ok, _PeerID} = handshake(Socket, InfoHash),
  inet:setopts(Socket, [{packet, 4}]),
  master ! {socket, Socket},
  loop(Socket).


% This loop handles keepalives, closed connections and passes on messages to
% their own methods.
loop(Socket) ->
  receive

    % Keepalive
    {tcp,Socket,<<0,0,0,0>>} ->
      io:format("Got a keepalive~n"),
      send_keepalive(Socket),
      bittorrent:loop(Socket);

    % Peer wire message
    {tcp,Socket,Message} ->
      handle_message(Message),
      bittorrent:loop(Socket);

    % Closed connection
    {tcp_closed,Socket} ->
      master ! closed,
      io:format("Connection was closed~n");

    % Unknown message
    Unknown ->
      io:format("Got ~p~n", [Unknown]),
      bittorrent:loop(Socket)

  end.


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


%
% RECEIVING MESSAGES
%

handle_message(<<>>) ->
  ok;

% 0. Choke
handle_message(<<0>>) ->
  io:format("You are choked~n");

% 1. Unchoke
handle_message(<<1>>) ->
  master ! unchoked,
  io:format("You are unchoked~n");

% 2. Interested
handle_message(<<2>>) ->
  io:format("The peer is interested~n");

% 3. Uninterested
handle_message(<<3>>) ->
  io:format("The peer is uninterested~n");

% 4. Have
handle_message(<<4,Payload:4/binary>>) ->
  PieceNumber = multibyte:binary_to_multibyte_integer(Payload),
  master ! {have, PieceNumber},
  io:format("The peer has piece ~p~n", [PieceNumber]);

% 5. Bitfield
handle_message(<<5,Payload/binary>>) ->
  master ! received_bitfield,
  io:format("Received bitfield with payload ~p~n", [Payload]),
  handle_message(<<>>);

% 6. Request
handle_message(<<6,PieceIndex:4/binary,BlockOffset:4/binary,BlockLength:4/binary>>) ->
  io:format("Received request for ~p bytes ~p bytes into piece ~p~n", [
      multibyte:binary_to_multibyte_integer(BlockLength),
      multibyte:binary_to_multibyte_integer(BlockOffset),
      multibyte:binary_to_multibyte_integer(PieceIndex)
  ]),
  master ! {received_requst, PieceIndex, BlockOffset, BlockLength};

% 8. Cancel
handle_message(<<8,PieceIndex:4/binary,BlockOffset:4/binary,BlockLength:4/binary>>) ->
  io:format("Had request cancelled for ~p bytes ~p bytes into piece ~p~n", [
      multibyte:binary_to_multibyte_integer(BlockLength),
      multibyte:binary_to_multibyte_integer(BlockOffset),
      multibyte:binary_to_multibyte_integer(PieceIndex)
  ]);

% Other
handle_message(Other) ->
  io:format("Got a weird message: ~p~n", [Other]).

%
% SENDING MESSAGES
%

% Sends a message to keep the connection open.
send_keepalive(Socket) ->
  io:format("Sending keepalive~n"),
  gen_tcp:send(Socket, <<0,0,0,0>>).

% 0. Choke
send_choke(Socket) ->
  io:format("Sending Choke~n"),
  gen_tcp:send(Socket, <<0>>).

% 1. Unchoke
send_unchoke(Socket) ->
  io:format("Sending unchoke~n"),
  gen_tcp:send(Socket, <<1>>).

% 2. Interested
send_interested(Socket) ->
  io:format("Sending interested~n"),
  gen_tcp:send(Socket, <<2>>).

% 3. Uninterested
send_uninterested(Socket) ->
  io:format("Sending uninterested~n"),
  gen_tcp:send(Socket, <<3>>).

% 4. Have
send_have(Socket, PieceIndex) ->
  MultiByteIndex = multibyte:number_to_multibyte_integer(PieceIndex, 4),
  io:format("Sending have ~p~n", [MultiByteIndex]),
  gen_tcp:send(Socket, list_to_binary([4,MultiByteIndex ])).

% 5. Bittfield
send_bitfield(Socket, Bitfield) ->
  io:format("Sending bittfield of ~p~n", [Bitfield]),
  Payload = list_to_binary([ 5, Bitfield ]),
  gen_tcp:send(Socket, Payload).

% 6. Request
send_request(Socket, PieceIndex, BlockOffset, BlockLength) ->
  io:format("Sending request for ~p~n", [PieceIndex]),
  gen_tcp:send(Socket, <<6,PieceIndex,BlockOffset,BlockLength>>).

% 7. Piece
% Piece index in 4, Block Offset is 4, Block data is length(), ID is 1
send_piece(Socket, PieceIndex, BlockOffset, BlockData) ->
  io:format("Sending from ~p to ~p of piece ~p~n",
    [multibyte:binary_to_multibyte_integer(BlockOffset),
     length(BlockData),
     multibyte:binary_to_multibyte_integer(PieceIndex)]),
  Payload = list_to_binary([7, PieceIndex, BlockOffset, BlockData]),
  gen_tcp:send(Socket, Payload).

