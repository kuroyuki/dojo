%%%-------------------------------------------------------------------
%%% @author Yuki
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. May 2015 21:00
%%%-------------------------------------------------------------------
-module(dojo_node).
-author("Yuki").
%% API
-export([start_link/1, init/1]).
-include("dojo.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start_link(NodeSavedData)->
  spawn_link(?MODULE, init, [NodeSavedData]).

init(NodeSavedData)->

  %restore node parameters
  NodeData = #node{
    id=NodeSavedData#saved_node.id,
    size = NodeSavedData#saved_node.size,
    position = NodeSavedData#saved_node.position,
    axon = NodeSavedData#saved_node.axon
  },
  dojo_logger:log(["started node ", NodeData]),

  %start targets seeking
  erlang:send_after(10000, self(), {find_targets, 2}),
  %start nood loop
  erlang:send_after(?NODE_TIMEOUT, self(), {timeout}),
  %run node loop
  loop(NodeData).
%%%===================================================================
%%% Main loop
%%%===================================================================
loop(Node)->
  receive
    {ap, From} ->
      case lists:keytake(From, #source.id, Node#node.sources) of
        false ->
          dojo_logger:log(["ap from", From ," - not found"]),
          loop(Node);
        {value, Source, Sources} ->
          UpdatedSource = Source#source{cleft = Source#source.cleft+1, lastAP = 1},
          loop(Node#node{sources = lists:keystore(From, #source.id, Sources, UpdatedSource)})
      end;

    {add_source, Source} ->
      case lists:keymember(Source#source.id, #source.id, Node#node.sources) of
        false ->
          dojo_logger:log(["added source ",Source]),
          loop(Node#node{sources = lists:keystore(Source#source.id, #source.id, Node#node.sources, Source)});
        true ->
          loop(Node)
      end;

    {add_target, Target} ->
      case lists:member(Target, Node#node.targets) of
        true ->
          loop(Node);
        false ->
          dojo_logger:log(["added target ",Target]),
          loop(Node#node{targets = [Target | Node#node.targets]})
      end;

    {remove_source, Source} ->
      loop(Node#node{sources = lists:keydelete(Source#source.id, #source.id, Node#node.sources)});

    {remove_target, Target} ->
      loop(Node#node{targets = lists:delete(Target, Node#node.targets)});

    {get_voltage, From} ->
      From !{node_voltage, Node#node.id, Node#node.voltage},
      loop(Node);

    {find_targets, AtDistance} ->
      {AxonX, AxonY, AxonZ} = Node#node.axon,
      PossibleTargets = dojo_storage:get_point_neigbours({AxonX, AxonY, AxonZ}, AtDistance),

      %sort by length
      LengthFun = fun(N, List) ->
        [TargetId, TargetX, TargetY, TargetZ] = N,
        {DiffX,DiffY,DiffZ} = {TargetX-AxonX, TargetY-AxonY, TargetZ-AxonZ},

        DiffXY = math:sqrt(abs(DiffX*DiffX)+abs(DiffY*DiffY)),

        Length = math:sqrt(abs(DiffXY*DiffXY)+abs(DiffZ*DiffZ)),

        if
          TargetId =/= Node#node.id ->
            if
              Length =< AtDistance ->
                [TargetId | List];
              Length > AtDistance ->
                List
            end;
          TargetId == Node#node.id ->
            List
        end
      end,
      SortedTargets = lists:foldr(LengthFun, [], PossibleTargets),

      SendFun = fun(X) ->
        dojo_server:bind_nodes(Node#node.id, X, 1)
      end,
      lists:foreach(SendFun, SortedTargets),

      %next iteration
      erlang:send_after(10000, self(), {find_targets, AtDistance}),
      loop(Node);

  {timeout} ->
    {UpdateSources, SynapticVoltage} = update_sources(Node#node.sources),
    NewVoltage = Node#node.voltage + SynapticVoltage,

    if NewVoltage >= ?VOLTAGE_THRESHOLD ->
        SendAp = fun(TargetPid)->
          TargetPid!{ap, self()}
        end,
        lists:foreach(SendAp, Node#node.targets),
        erlang:send_after(?NODE_TIMEOUT, self(), {timeout}),
        loop(Node#node{voltage = ?VOLTAGE_STEADY, sources = UpdateSources});

      NewVoltage < -1 ->
        erlang:send_after(?NODE_TIMEOUT, self(), {timeout}),
        loop(Node#node{voltage = -1, sources = UpdateSources});

      NewVoltage < 1 ->
        erlang:send_after(?NODE_TIMEOUT, self(), {timeout}),
        loop(Node#node{voltage = NewVoltage*?PUMPING_CONST, sources = UpdateSources})
    end
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

update_sources([]) ->
  {[],0};
update_sources(List) ->
  %get source from list
  update_sources([], 0, List).

update_sources(UpdateList, SynapticVoltage, [])->
  {UpdateList, SynapticVoltage};
update_sources(UpdateList, SynapticVoltage, List)->
  %get source from list
  [Source | RemainList] = List,
  %if source has voltage in cleft
  case Source#source.cleft /= 0 of
    true ->
      %used voltage according to time course
      ReleasedTransmitter = Source#source.cleft*(1-math:exp(Source#source.lastAP*?TIME_CONST)),
      %substract used voltage from cleft and save reminder, update time after ap
      UpdatedSource = Source#source{cleft = Source#source.cleft-ReleasedTransmitter, lastAP = Source#source.lastAP+10},

      %%voltage changes from this synapse
      SynVoltage = Source#source.coefficient*ReleasedTransmitter*math:exp(Source#source.length*?LENGTH_CONST),

      update_sources([UpdatedSource|UpdateList],
        SynapticVoltage+SynVoltage,
        RemainList);
    %source has no voltage
    false ->
      %to the next
      UpdatedSource = Source#source{lastAP = Source#source.lastAP+10},
      update_sources([UpdatedSource|UpdateList], SynapticVoltage, RemainList)

  end.
