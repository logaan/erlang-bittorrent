-module(bittorrent).
-export([start_peer/3]).
-compile(export_all).

% Spawns a new peer process and returns the pid.
start_peer(Host, Port, InfoHash) ->
  spawn_link(?MODULE, start_loop, [Host, Port, InfoHash]).


% Creates the connection to the peer and handshakes before handing off to the
% main loop.
start_loop(Host, Port, InfoHash) ->
  io:format("Starting loop for ~p~n", [InfoHash]),
  {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}]),
  {ok, _PeerID} = handshake(Socket, InfoHash),
  send_bitfield(Socket, [255,239,248,255,201,38,102,30,141,233,194,255,66,27,39,218,115,174,96,87,252,102,255,16,192,223,128,94,248,105,176,163,252,29,66,168,255,48,130,225,50,118,64,168,38,68,64,42,21,173,32,47,133,142,94,48,255,73,131,144,255,255,255,255,200,110,68,229,246,192,65,76,42,14,19,118,146,138,201,186,144,94,44,84,244,200,204,184,150,182,102,240,194,169,116,50,165,12,21,30,89,224,29,37,87,59,124,3,16,163,130,178,242,215,66,240,5,175,37,31,97,130,68,165,4,177,66,222,131,78,215,40,88,31,239,244,168,201,144,29,255,29,115,80,201,96,146,44,112,98,112,132,55,168,34,65,129,51,120,185,201,6,238,175,124,24,202,140,226,164,81,12,244,245,87]),
  send_unchoke(Socket),
  loop(Socket).


% This loop handles keepalives, closed connections and passes on messages to
% their own methods.
loop(Socket) ->
  receive

    % Keepalive
    {tcp,Socket,<<0,0,0,0,0,0,0,0>>} ->
      io:format("Got a keepalive~n"),
      send_keepalive(Socket),
      bittorrent:loop(Socket);

    % Peer wire message
    {tcp,Socket,<<MessageLength:4/binary, MessageID/integer, Tail/binary>>} ->
      handle_message(list_to_binary([MessageLength, MessageID, Tail])),
      bittorrent:loop(Socket);

    % Closed connection
    {tcp_closed,Socket} ->
      io:format("Connection was closed~n");

    % Socket request
    {socket,Sender} ->
      Sender ! Socket,
      bittorrent:loop(Socket);

    % Unknown message
    Unknown ->
      io:format("Got ~p~n", [Unknown]),
      bittorrent:loop(Socket)

  end.


% Each message has it's own method. Any data tacked onto the end of a message is
% passed back into this method until there is no more data, which returns ok.
handle_message(<<>>) ->
  ok;

% Unchoke
handle_message(<<0,0,0,1,1,Tail/binary>>) ->
  io:format("You are unchoked~n"),
  handle_message(Tail);

% Interested
handle_message(<<0,0,0,1,2,Tail/binary>>) ->
  io:format("The peer is interested~n"),
  handle_message(Tail);

% Have
handle_message(<<0,0,0,5,4,Payload:4/binary, Tail/binary>>) ->
  PieceNumber = multibyte:binary_to_multibyte_integer(Payload),
  io:format("The peer has piece ~p~n", [PieceNumber]),
  handle_message(Tail);

% Bitfield
handle_message(<<MessageLength:4/binary,5,Tail/binary>>) ->
  Length  = multibyte:binary_to_multibyte_integer(MessageLength) - 1,
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

% Unchoke
send_unchoke(Socket) ->
  gen_tcp:send(Socket, <<0,0,0,1,1>>).

% Interested
send_interested(Socket) ->
  gen_tcp:send(Socket, <<0,0,0,1,2>>).

% Request
send_request(Socket, PieceIndex, BlockOffset, BlockLength) ->
  gen_tcp:send(Socket, <<0,0,0,13,6,PieceIndex,BlockOffset,BlockLength>>).

send_bitfield(Socket, Bitfield) ->
  Payload = list_to_binary([
    multibyte:number_to_multibyte_integer(length(Bitfield) + 1,4),
    5,
    Bitfield
  ]),
  gen_tcp:send(Socket, Payload).

