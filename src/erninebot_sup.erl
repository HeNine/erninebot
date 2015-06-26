-module(erninebot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
  {ok, {{one_for_one, 5, 10},
    [
      ?CHILD(enb_filter_chain_sup, supervisor),
      ?CHILD(enb_message_exchange_srv, worker),

      ?CHILD(enb_console_log_srv, worker),
      ?CHILD(enb_parser_srv, worker),
      ?CHILD(enb_ping_srv, worker),
      ?CHILD(enb_channel_srv, worker),
      ?CHILD(enb_io_srv, worker),

      ?CHILD(erninebot_service_sup, supervisor)
    ]
  }}.

