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
  Node = #node{
    id=NodeSavedData#saved_node.id,
    size = NodeSavedData#saved_node.size,
    position = NodeSavedData#saved_node.position,
    axon = NodeSavedData#saved_node.axon
  },
  ?Log(["added node ", Node]),
  %run node loop
  loop(Node).
%%%===================================================================
%%% Main loop
%%%===================================================================
loop(Node)->
  receive
    {ap, From} ->
      case lists:keytake(From, #source.id, Node#node.sources) of
        false ->
          ?Log(["ap from", From ," - not found"]),
          loop(Node);
        {value, Source, Sources} ->
          UpdatedSource = Source#source{cleft = Source#source.cleft+1, lastAP = 1},
          loop(Node#node{sources = lists:keystore(From, #source.id, Sources, UpdatedSource)})
      end;

    {add_source, Source} ->
      ?Log(["added source ",Source]),
      loop(Node#node{sources = lists:keystore(Source#source.id, #source.id, Node#node.sources, Source)});

    {add_target, Target} ->
      ?Log(["added target ",Target]),
      case lists:member(Target, Node#node.targets) of
        true ->
          loop(Node);
        false ->
          loop(Node#node{targets = [Target | Node#node.targets]})
      end;

    {remove_source, Source} ->
      loop(Node#node{sources = lists:keydelete(Source#source.id, #source.id, Node#node.sources)});

    {remove_target, Target} ->
      loop(Node#node{targets = lists:delete(Target, Node#node.targets)});

    {get_voltage, From} ->
      From !{node_voltage, Node#node.id, Node#node.voltage}

  after 10 ->
    {UpdateSources, SynapticVoltage} = update_sources(Node#node.sources),
    NewVoltage = Node#node.voltage + SynapticVoltage,

    if NewVoltage >= ?VOLTAGE_THRESHOLD ->
        generate_ap(Node#node.targets),
        loop(Node#node{voltage = ?VOLTAGE_STEADY, sources = UpdateSources});
      NewVoltage < 1 ->
        loop(Node#node{voltage = NewVoltage, sources = UpdateSources})
    end
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

generate_ap([])->
  ok;
generate_ap(TargetList)->
  [Target | RemainList] = TargetList,
  ?Log(["ap generated to ", Target]),
  Target ! {ap, self()},
  generate_ap(RemainList).

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
      SynVoltage = Source#source.cleft*(1-math:exp(Source#source.lastAP*?TIME_CONST)),
      %substract used voltage from cleft and save reminder, update time after ap
      UpdatedSource = Source#source{cleft = Source#source.cleft-SynVoltage, lastAP = Source#source.lastAP+10},
      %%
      update_sources([UpdatedSource|UpdateList],
        SynapticVoltage+SynVoltage*math:exp(Source#source.length*?LENGTH_CONST),
        RemainList);
  %source has no voltage
    false ->
      %to the next
      UpdatedSource = Source#source{lastAP = Source#source.lastAP+10},
      update_sources([UpdatedSource|UpdateList], SynapticVoltage, RemainList)

  end.
