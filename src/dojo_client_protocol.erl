%%%-------------------------------------------------------------------
%%% @author Yuki
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2015 18:25
%%%-------------------------------------------------------------------
-module(dojo_client_protocol).
-behaviour(ranch_protocol).

-include("dojo.hrl").

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
  {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
  ok = ranch:accept_ack(Ref),
  ?Log(["accepted TCP"]),

  loop(Socket, Transport).

loop(Socket, Transport) ->
  case Transport:recv(Socket, 0, 5000) of
    {ok, Data} ->
      ?Log(["received TCP data ",Data]),
      {ok, {ClientHost, _ClientPort}} = Transport:peername(Socket),
      parse_tcp_binary(Data, ClientHost),
      loop(Socket, Transport);
    {error, timeout} ->
      loop(Socket, Transport);
    Any ->
      ?Log(["Unknown Tcp mesage~p~n", Any]),
      ok = Transport:close(Socket)
  end.

parse_tcp_binary(<<>>, _Ip) ->
  ok;
parse_tcp_binary(BinData, ClientHost)->
  <<Command:8, Data:32, RemainBin/binary>> = BinData,
  case Command of
    %add input
    0 ->
      dojo_server:register_sensor(ClientHost, Data);
    1 ->
      dojo_server:register_actuator(Data, ClientHost);
    Any ->
      ?Log(["unknown TCP command ", Any])
  end,
  parse_tcp_binary(RemainBin, ClientHost).
