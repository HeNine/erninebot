-module(erninebot_app).

-behaviour(application).

%% application Function Exports
-export([
  start/0,
  start/2,
  stop/1]).

%% application Function Definitions

%% @private
%% @doc
%% Application start
%%
%% @end
-spec(start() ->
  {ok, pid()} | {ok, pid(), term()} | {error, term()}).
start() ->
  start(normal, []).

-spec(start(normal|{takeover|failover, node()}, term()) ->
  {ok, pid()} | {ok, pid(), term()} | {error, term()}).

start(_StartType, _StartArgs) ->
  erninebot_sup:start_link().

%% @private
%% @doc
%% Application stop
%%
%% @end
-spec(stop(term()) ->
  ignored).

stop(_State) ->
  ok.
