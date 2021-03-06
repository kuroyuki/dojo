%%%-------------------------------------------------------------------
%%% @author Yuki
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. May 2015 0:38
%%%-------------------------------------------------------------------
-author("Yuki").

-define(VOLTAGE_THRESHOLD, 1).
-define(VOLTAGE_STEADY, 0).

-define(TIME_CONST, -0.02). %in format -1/50, where 50 is time const in ms
-define(LENGTH_CONST, -1). %in format -1/1, where 1 is length const in mm
-define(PUMPING_CONST, 0.95).
-define(NODE_TIMEOUT, 10).

-define(TCP_SERVER_PORT, 5555).
-define(UDP_CLIENT_PORT, 49389).
-define(UDP_SERVER_PORT, 45907).

-record(dojo_state, {next_node, udp_socket}).

-record(node_process, {id, process}).
-record(node, {id, voltage=?VOLTAGE_STEADY, sources = [], targets = [], size, position, axon}).
-record(source, {id, length, coefficient, terminals=1, cleft=0, lastAP=1}).
-record(udp_io, {pid, udp_host, id}).

-record(saved_node, {id, size, position, axon}).
-record(saved_synapse, {id, coefficient}).



