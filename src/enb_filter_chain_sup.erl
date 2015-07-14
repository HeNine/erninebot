%% @doc
%% Supervisor for filter chains.
%%
%% @since 1.0
%% @end
-module(enb_filter_chain_sup).
-behaviour(supervisor).
-define(CHILD(Name, Module, Args, Type), {Name, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%% API Function Exports
-export([
  start_link/0,
  add_chain/2]).

%% supervisor Function Exports
-export([init/1]).

%% API Function Definitions

%% @doc
%% Starts the supervisor and returns its pid.
%%
%% @since 1.0
%% @end
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc
%% Creates a new filter chain server and adds it to the supervisor.
%%
%% @since 1.0
%% @end
-spec(add_chain(Name :: atom(), ChainSpec :: list(mfa())) ->
  pid()).

add_chain(Name, ChainSpec) ->
  {ok, Child} = supervisor:start_child(?MODULE, [Name, ChainSpec]),
  Child.

%% supervisor Function Definitions

%% @private
%% @doc
%% Initializes the supervisor
%%
%% @end
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} | ignore | {error, Reason :: term()}).

init(_) ->
  {ok, {{simple_one_for_one, 5, 10},
    [{id,
      {enb_filter_chain_srv, start_link, []},
      permanent,
      5000,
      worker,
      [enb_filter_chain_srv]}]
  }}.
