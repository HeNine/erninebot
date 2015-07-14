%% @doc
%% The ping server
%%
%% This server takes care of replying to heartbeat pings sent from the IRC server.
%%
%% @since 1.0
%% @end
-module(enb_ping_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {nickname}).

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
%% Server initialization
%%
%% @end
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} | {stop, Reason :: term()} | ignore).

init(_) ->
  {ok, Nickname} = application:get_env(erninebot, nickname),
  enb_message_exchange_srv:subscribe(ping),
  {ok, #state{nickname = Nickname}}.

%% @hidden
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @private
%% @doc
%% One receiving a PING command replies with a pong
%%
%% @end
-spec(handle_cast(Message :: {message, #message{}}, State :: #state{}) ->
  {noreply, State :: #state{}}).

handle_cast({message, #message{command = ping, last_param = Server}}, State = #state{nickname = Nickname}) ->
  enb_message_exchange_srv:send_message(#message{command = pong, params = [Nickname], last_param = Server}),
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
