%% @doc
%% The message exchage server is the central server of the bot.
%%
%% This server recieves raw messages from the IO server and parses them. Then it distributes the message to subscribers.
%%
%% ===Filter chains===
%% Any server may subscribe to messages. The chosen filter chain determines which messages it receives. By default, the
%% chains for all messages and bot commands are created. Chains for individual IRC message commands are created on
%% requst.
%%
%% @todo: Support for other filter functions.
%% @todo: Define behaviour in case the same server would receive the same message from multiple chains.
%% @since 1.0
%% @end
-module(enb_message_exchange_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {subscriptions = maps:new(), filter_chains}).

-include("message.hrl").

%% API Function Exports
-export([
  start_link/0,
  send_message/1,
  subscribe/1]).

%% gen_server Function Exports
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%% API Function Definitions

%% @doc
%% Starts the server and returns its pid.
%%
%% @since 1.0
%% @end
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%% Utility function for sending a message to the IRC server.
%%
%% @since 1.0
%% @end
-spec(send_message(Message :: #message{}) -> ok).

send_message(Message) ->
  gen_server:cast(enb_message_exchange_srv, {message_out, Message}).

%% @doc
%% Functions for subscribing to filter chains
%%
%% The atom 'all' subscribes to all messages, 'bot_command' subscribes to bot commands. Any other atom is treated as an
%% IRC command as described in  {@link enb_message_parser}.
%%
%% @since 1.0
%% @end
-spec(subscribe(atom()) -> ok).

subscribe(all) ->
  gen_server:cast(enb_message_exchange_srv, {subscribe, self(), all});
subscribe(bot_command) ->
  gen_server:cast(enb_message_exchange_srv, {subscribe, self(), bot_command});
subscribe(Command) ->
  ok = gen_server:call(
    enb_message_exchange_srv,
    {create_chain, Command, [{enb_basic_filters, command_filter, [Command]}]}),
  gen_server:cast(enb_message_exchange_srv, {subscribe, self(), Command}).

%% gen_server Function Definitions

%% @private
%% @doc
%% Server initialization. Creates the default filter chains.
%%
%% @end
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} | {stop, Reason :: term()} | ignore).

init(_Args) ->
  Chains = maps:from_list([
    {all, enb_filter_chain_sup:add_chain(all, [{enb_basic_filters, all_filter, []}])},
    {bot_command, enb_filter_chain_sup:add_chain(bot_command, [{enb_basic_filters, bot_command_filter, []}])}
  ]),
  {ok, #state{filter_chains = Chains}}.

%% @private
%% @doc
%% Handles filter chain creation.
%%
%% First checks if the chain already exists. Then
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

%% @private
%% @doc
%% Handles messages and subscriptions
%%
%% This functionality shlould be used through provided API functions.
%%
%% @end
-spec(handle_cast(Message :: {message, #message{}}, State :: #state{}) ->
  {noreply, State :: #state{}}).

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
  io:format("~s", [RawMessage]),
  gen_server:cast(enb_io_srv, {send_message, RawMessage}),
  {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
  {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
  ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal Function Definitions

%% @private
%% @doc
%% Auxiliary function used to distribute the message to subscribed servers.
%%
%% @end
distribute(Message, Targets, Filter) ->
  case gen_server:call(Filter, {filter, Message}) of
    filtered -> ok;
    _ -> lists:map(fun(Pid) -> gen_server:cast(Pid, {message, Message}) end, Targets)
  end.
