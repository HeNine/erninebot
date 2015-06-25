-module(enb_filter_chain_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_chain/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Module, Args, Type), {Name, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_chain(Name, ChainSpec) ->
  {ok, Child} = supervisor:start_child(enb_filter_chain_sup, [Name, ChainSpec]),
  Child.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_) ->
  {ok, {{simple_one_for_one, 5, 10},
    [{id,
      {enb_filter_chain_srv, start_link, []},
      permanent,
      5000,
      worker,
      [enb_filter_chain_srv]}]
  }}.
