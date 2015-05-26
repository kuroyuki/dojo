-module(dojo_app).

-behaviour(application).

-include("dojo.hrl").
%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

  {ok, _} = ranch:start_listener(dojo_client, 10,
    ranch_tcp, [{port, ?TCP_SERVER_PORT}], dojo_client_protocol, []),

  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, dojo, "index.html"}},
      {"/websocket", dojo_ws_handler, []},
      {"/static/[...]", cowboy_static, {priv_dir, dojo, "static"}}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]),

  dojo_sup:start_link().

stop(_State) ->
    ok.
