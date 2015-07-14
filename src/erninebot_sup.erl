%% @doc
%% Main supervisor
%%
%% The application's main supervisor. This supervisor takes care of starting the main servers and other supervisors.
%%
%% @todo: Analyze server starting order
%% @since 1.0
%% @end
-module(erninebot_sup).
-behaviour(supervisor).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% API Function Exports
-export([start_link/0]).

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

