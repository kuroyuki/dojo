%%%-------------------------------------------------------------------
%%% @author Yuki
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2015 9:19
%%%-------------------------------------------------------------------
-module(dojo_server).
-author("Yuki").

-include("dojo.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add_node/2]).
-export([register_sensor/2]).
-export([register_actuator/2]).

-export([bind_nodes/2]).

%debug
-export([create_nodes/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_node(NodeCoords,AxonCoords)->
  gen_server:call(?SERVER, {add_node, NodeCoords, AxonCoords}).

bind_nodes(Source,Target)->
  gen_server:call(?SERVER, {bind_nodes, Source, Target}).

register_sensor(SourceHostIp, TargetNodeId)->
  gen_server:cast(?SERVER, {add_sensor, SourceHostIp, TargetNodeId}).
register_actuator(SourceNodeId, TargetHostIp)->
  gen_server:cast(?SERVER, {add_actuator, SourceNodeId, TargetHostIp}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
init([]) ->
  ets:new(node_processes, [set, named_table, {keypos,#node_process.id}]),
  %register itself
  ets:insert_new(node_processes, #node_process{id=dojo_server, process = self()}),

  ets:new(udp_io, [set, named_table, {keypos,#udp_io.pid}]),

  %restore saved config
  gen_server:cast(?SERVER, {restore_dojo}),

  %start UDP server for IO
  {ok, UdpSocket} = gen_udp:open(?UDP_SERVER_PORT, [binary]),

  dojo_logger:log(["init dojo_server"]),
  %we count nodfes start with 1, due to QHash specific behaviour with id 0
  {ok, #dojo_state{next_node=0, udp_socket=UdpSocket}}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({add_node, NodeCoords, AxonCoords}, _From, State) ->

  NodeId = State#dojo_state.next_node,

  %save node to dojo_storage
  Node = #saved_node{id=NodeId, position = NodeCoords,axon = AxonCoords, size = 1},
  dojo_storage:add_node(Node),

  %start new node process
  NodeProcess = dojo_node:start_link(Node),

  %save node process
  ets:insert_new(node_processes, #node_process{id=NodeId, process = NodeProcess}),

  {reply, ok, State#dojo_state{next_node = NodeId +1}};

handle_call({bind_nodes, Source, Target}, _From, State) ->
  SavedSynapse = #saved_synapse{id={Source,Target}, permability = 1},

  case start_synapse(SavedSynapse) of
    ok ->
      dojo_storage:add_synapse(SavedSynapse),
      {reply, ok, State};
    Any ->
      dojo_logger:log(["synapse ",SavedSynapse," not added, reason ", Any]),
      {reply, Any, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, ok, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({restore_dojo}, State) ->
  %restore saved nodes
  NodesList = dojo_storage:get_all_nodes(),
  NextNode = restore_nodes(NodesList, 0),
  dojo_logger:log(["restored nodes ", NodesList]),

  %restore saved synapses
  SynapsesList = dojo_storage:get_all_synapses(),
  dojo_logger:log(["restored synapses", SynapsesList]),
  restore_synapses(SynapsesList),

  {noreply, State#dojo_state{next_node=NextNode}};
handle_cast( {add_sensor, SourceHostIp, TargetNodeId}, State) ->

  SavedSynapse = #saved_synapse{id={dojo_server,TargetNodeId}, permability = 1},

  case start_synapse(SavedSynapse) of
    ok ->
      dojo_logger:log(["sensor",SavedSynapse, " created"]);
    Any ->
      dojo_logger:log(["sensor ",SavedSynapse, " not created, reason ", Any])
  end,
  {noreply, State};
handle_cast({add_actuator, SourceNodeId, TargetHostIp}, State) ->
  %add source to node
  SavedSynapse = #saved_synapse{id={SourceNodeId, dojo_server}, permability = TargetHostIp},
   case start_synapse(SavedSynapse) of
    ok ->
      dojo_logger:log(["actuator ",SavedSynapse," created"]);
    Any ->
      dojo_logger:log(["actuator ",SavedSynapse," created, reason ", Any])
  end,
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_info({udp, _Socket, Host, Port, Bin}, State) ->
  parse_udp_binary(Bin),
  {noreply, State};
handle_info({ap, From}, State) ->
  case ets:lookup(udp_io, From) of
    [] ->
      dojo_logger:log([From, " pid isn't register for UDP as output ~n"]),
      not_found;
    [{udp_io, From, ClientIp, OutputId}] ->
      Bin = <<OutputId:32>>,
      gen_udp:send(State#dojo_state.udp_socket, ClientIp, ?UDP_CLIENT_PORT, Bin)
  end,
  {noreply, State};
handle_info(Info, State) ->
  dojo_logger:log(["unknown message ",Info]),
  {noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
restore_nodes([], NextNode)->
  NextNode;
restore_nodes(NodesList, NextNode)->
  [[SavedNode] | RemainNodes] = NodesList,

  %start new node process
  NodeProcess = dojo_node:start_link(SavedNode),

  %save node process
  NodeId = SavedNode#saved_node.id,
  ets:insert_new(node_processes, #node_process{id=NodeId, process = NodeProcess}),

  %update next node id
  if NextNode > NodeId ->
      restore_nodes(RemainNodes, NextNode);
    NextNode =< NodeId ->
      restore_nodes(RemainNodes, NodeId)
  end.

restore_synapses([])->
  ok;
restore_synapses(SynapsesList)->
  [[SavedSynapse] | RemainList] = SynapsesList,
  start_synapse(SavedSynapse),
  restore_synapses(RemainList).

start_synapse(SavedSynapse)->
  %get source and target ids
  {SourceId, TargetId} = SavedSynapse#saved_synapse.id,

  %get source Pid
  case ets:lookup(node_processes, SourceId) of
    [] ->
      not_found;
    [{node_process, dojo_server, SourcePid}] ->
      case ets:lookup(node_processes, TargetId) of
        [] ->
          not_found;
        [{node_process, TargetId, TargetPid}] ->
          %save source on target
          Source = #source{
            id = SourcePid,
            permability = SavedSynapse#saved_synapse.permability,
            length = 0.5
          },
          TargetPid!{add_source, Source},
          ok
      end;
    [{node_process, SourceId, SourcePid}] ->
      %get target Pid
      case ets:lookup(node_processes, TargetId) of
        [] ->
          not_found;
        [{node_process, dojo_server, TargetPid}] ->
          %save target IP on itself
          ets:insert_new(udp_io, #udp_io{pid=SourcePid, udp_host = SavedSynapse#saved_synapse.permability, id=SourceId}),
           %save target on source
          SourcePid!{add_target, TargetPid},
          ok;
        [{node_process, TargetId, TargetPid}] ->
          %save target on source
          SourcePid!{add_target, TargetPid},

          {ok, SourceNodeData} = dojo_storage:get_node_params(SourceId),
          {ok, TargetNodeData} = dojo_storage:get_node_params(TargetId),

          %calculate length between Sorce Axon position and target node position
          {Ax, Ay, Az} = SourceNodeData#saved_node.axon,
          {Bx, By, Bz} = TargetNodeData#saved_node.position,

          {DiffX,DiffY,DiffZ} = {Bx-Ax, By-Ay, Bz-Az},

          DiffXY = math:sqrt(abs(DiffX*DiffX)+abs(DiffY*DiffY)),

          Length = math:sqrt(abs(DiffXY*DiffXY)+abs(DiffZ*DiffZ)),

          %save source on target
          Source = #source{
            id = SourcePid,
            permability = SavedSynapse#saved_synapse.permability,
            length = Length
          },
          TargetPid!{add_source, Source},
          ok
      end
  end.

create_nodes(0) ->
  ok;
create_nodes(MaxNodes)->
  add_node({random:uniform(1000), random:uniform(1000), random:uniform(1000)},
    {random:uniform(1000),random:uniform(1000),random:uniform(1000)}),
  create_nodes(MaxNodes-1).

parse_udp_binary(<<>>) ->
  ok;
parse_udp_binary(Bin) ->
  <<TargetId:32, RemainBin/binary>> = Bin,
  case ets:lookup(node_processes, TargetId) of
    [] ->
      dojo_logger:log(["no pid found for id ", TargetId]),
      not_found;
    [{node_process, TargetId, TargetPid}] ->
      TargetPid!{ap,self()}
  end,
  parse_udp_binary(RemainBin).

