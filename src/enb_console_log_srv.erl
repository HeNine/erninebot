%% @doc
%% This server is used to print messages to the console.
%%
%% @since 1.0
-module(enb_console_log_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {}).

-include("message.hrl").

%% API Function Exports
-export([start_link/0]).

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

%% gen_server Function Definitions

%% @private
%% @doc
%% Initializes the server and subscribes to all mesages.
%%
%% @end
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} | {stop, Reason :: term()} | ignore).

init(_Args) ->
  enb_message_exchange_srv:subscribe(all),
  {ok, #state{}}.

%% @hidden
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @private
%% @doc
%% Takes a message and prints it in its raw form to the terminal.
%%
%% @end
handle_cast({message, #message{original = RawMessage}}, State) ->
  io:format("~s", [RawMessage]),
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

