%%%-------------------------------------------------------------------
%%% @author Yuki
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. May 2015 9:19
%%%-------------------------------------------------------------------
-module(dojo_storage).
-author("Yuki").

-behaviour(gen_server).
-include("dojo.hrl").

%% API
-export([start_link/0]).
-export([add_node/1, get_node_params/1, get_all_nodes/0, remove_node/1]).
-export([add_synapse/1, remove_synapse/1, get_all_synapses/0]).

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

add_node(SavedNode)->
  gen_server:cast(?SERVER, {add_node, SavedNode}).

get_node_params(NodeId)->
  gen_server:call(?SERVER, {get_node_params, NodeId}).

get_all_nodes()->
  gen_server:call(?SERVER, {get_all_nodes}).

remove_node(NodeId)->
  gen_server:cast(?SERVER, {remove_node, NodeId}).

add_synapse(Synapse)->
  gen_server:cast(?SERVER, {add_synapse, Synapse}).

get_all_synapses()->
  gen_server:call(?SERVER, {get_all_synapses}).

remove_synapse(Synapse)->
  gen_server:cast(?SERVER, {remove_synapse, Synapse}).

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
  ?Log(["init dojo_storage"]),
  dets:open_file(nodes_table, [{keypos,#saved_node.id}]),
  dets:open_file(synapses_table, [{keypos, #saved_synapse.id}]),
  {ok, []}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call({get_node_params, NodeId}, _From, State) ->
  case dets:lookup(nodes_table, NodeId) of
    [Rec] ->
      {reply, {ok, Rec}, State};
    Any ->
      io:format("node ~p not found with ~p~n", [NodeId, Any]),
      {reply, {fail, not_found}, State}
  end;
handle_call({get_all_nodes}, _From, State)->
  Reply = dets:match(nodes_table,'$1'),
  {reply, Reply, State};
handle_call({get_all_synapses}, _From, State)->
  Reply = dets:match(synapses_table,'$1'),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({add_node, SavedNode}, State) ->
  dets:insert_new(nodes_table, SavedNode),
  {noreply, State};
handle_cast({remove_node, NodeId}, State) ->
  dets:delete(nodes_table, NodeId),
  {noreply, State};
handle_cast({add_synapse, SavedSynapse}, State) ->
  dets:insert_new(synapses_table, SavedSynapse),
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
handle_info(_Info, State) ->
  {noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  dets:close(nodes_table),
  dets:close(synapses_table),
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
