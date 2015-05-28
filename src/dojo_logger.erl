%%%-------------------------------------------------------------------
%%% @author Yuki
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. May 2015 1:24
%%%-------------------------------------------------------------------
-module(dojo_logger).
-author("Yuki").
%% API
-export([log/1]).

log(Message) ->
  {{Year,Month,Day},{Hour,Minute,Second}}=calendar:now_to_local_time(now()),
  {_,_, Microsecond} = now(),
  io:format("~p/~p/~p ~p:~p:~p.~p : ~p : ~p~n", [Year,Month,Day,Hour,Minute,Second,Microsecond, self(), Message]).

