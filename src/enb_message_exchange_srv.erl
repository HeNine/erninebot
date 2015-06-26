-module(enb_message_exchange_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("message.hrl").

-record(state, {subscriptions = maps:new(), filter_chains}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, send_message/1, subscribe/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  Chains = maps:from_list([
    {all, enb_filter_chain_sup:add_chain(all, [{enb_basic_filters, all_filter, []}])},
    {bot_command, enb_filter_chain_sup:add_chain(bot_command, [{enb_basic_filters, bot_command_filter, []}])}
  ]),
  {ok, #state{filter_chains = Chains}}.

handle_call({create_chain, Name, ChainSpec}, _From, State = #state{filter_chains = Chains}) ->
  NewChains = case maps:is_key(Name, Chains) of
                false ->
                  maps:put(Name, enb_filter_chain_sup:add_chain(Name, ChainSpec), Chains);
                true ->
                  Chains
              end,
  {reply, ok, State#state{filter_chains = NewChains}};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% Message for Pid to subscribe to Command messages
handle_cast({subscribe, Pid, Command}, State = #state{subscriptions = Subs}) ->
  Existing = maps:get(Command, Subs, []),
  {noreply, State#state{subscriptions = maps:put(Command, [Pid | Existing], Subs)}};

%% Incoming message to be distributed to subscribed servers
handle_cast({message_in, RawMessage}, State = #state{subscriptions = Subs, filter_chains = Chains}) ->
  Message = gen_server:call(enb_parser_srv, {parse, RawMessage}),
  maps:map(fun(Chain, FilterServer) -> distribute(Message, maps:get(Chain, Subs, []), FilterServer) end, Chains),
  {noreply, State};

%% Sends message Message
handle_cast({message_out, Message}, State) ->
  RawMessage = gen_server:call(enb_parser_srv, {unparse, Message}),
  gen_server:cast(enb_io_srv, {send_message, RawMessage}),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

send_message(Message) ->
  gen_server:cast(enb_message_exchange_srv, {message_out, Message}).

subscribe(all) ->
  gen_server:cast(enb_message_exchange_srv, {subscribe, self(), all});
subscribe(bot_command) ->
  gen_server:cast(enb_message_exchange_srv, {subscribe, self(), bot_command});
subscribe(Command) ->
  ok = gen_server:call(
    enb_message_exchange_srv,
    {create_chain, Command, [{enb_basic_filters, command_filter, [Command]}]}),
  gen_server:cast(enb_message_exchange_srv, {subscribe, self(), Command}).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

distribute(Message, Targets, Filter) ->
  case gen_server:call(Filter, {filter, Message}) of
    filtered -> ok;
    _ -> lists:map(fun(Pid) -> gen_server:cast(Pid, {message, Message}) end, Targets)
  end.