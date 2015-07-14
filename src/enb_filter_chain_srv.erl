%% @doc
%% Filter chain server is a gen_server, that can be called with a message and returns either the message or the atom
%% 'filtered', if one of the filters in the chain returns false.
%%
%% @since 1.0
%% @end
-module(enb_filter_chain_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {chain = []}).

-include("message.hrl").

%% API Function Exports
-export([start_link/2]).

%% gen_server Function Exports
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%% API Function Definitions

%% @doc
%% Starts the server and returns its pid.
%%
%% The Name argument is the name of the server. The ChainSpec argument is a list of @{M, F, A@} tuples used to call
%% filter functions. The message being filtered is prepended to A.
%%
%% @since 1.0
%% @end
-spec(start_link(Name :: atom(), ChainSpec :: list(mfa())) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link(Name, ChainSpec) ->
  gen_server:start_link({local, Name}, ?MODULE, ChainSpec, []).

%% gen_server Function Definitions

%% @private
%% @doc
%% Initializes the server
%%
%% @end
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init(Chain) ->
  {ok, #state{chain = Chain}}.

%% @private
%% @doc
%% Filters a message
%%
%% Message is checked against the chain of filters and the result is returned.
%%
%% @end
-spec(handle_call(Message :: {filter, #message{}}, _From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: ok | #message{} | filtered, NewState :: #state{}}).

handle_call({filter, Message}, _From, State = #state{chain = Chain}) ->
  {reply, filter(Message, Chain), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @hidden
handle_cast(_Request, State) ->
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
%% Accepts a message and a list of filter functions and applies them to the message. If any function returns 'false'
%% filter return the atom 'filtered', otherwise it returns the original message.
%%
%% @end
-spec(filter(#message{}, list(mfa())) ->
  #message{} | filtered).

filter(Message, [{M, F, A} | Chain]) ->
  case erlang:apply(M, F, [Message | A]) of
    true ->
      filter(Message, Chain);
    false ->
      filtered
  end;
filter(Message, []) ->
  Message.
