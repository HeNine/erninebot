%% @doc
%% Supervisor for services
%%
%% Service supervisor supervises the services offered by the bot to the users.
%%
%% @since 1.0
%% @end
-module(erninebot_service_sup).
-behaviour(supervisor).
-define(SERVER, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% API Function Exports
-export([start_link/0]).

%% supervisor Function Exports
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% API Function Definitions

%% @doc
%% Starts the supervisor and returns its pid.
%%
%% @since 1.0
%% @end
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
      ?CHILD(enb_echo_srv, worker),
      ?CHILD(enb_insult_srv, worker)
    ]
  }}.


%% Internal Function Definitions
