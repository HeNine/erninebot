%% @doc
%% Message persing server
%%
%% To ensure that the whole bot doesn't crash if a message fails to parse, the parsing functionality is available in a
%% server.
-module(enb_parser_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {}).

-include("message.hrl").

%% API Function Exports
-export([
  start_link/0,
  unparse/1]).

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
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%% Utility function for message unparsing
%%
%% @since 1.0
%% @end
-spec(unparse(Message :: #message{}) ->
  binary()).
unparse(Message) ->
  gen_server:call(enb_parser_srv, {unparse, Message}).

%% gen_server Function Definitions

%% @private
%% @doc
%% Server initialization
%%
%% @end
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} | {stop, Reason :: term()} | ignore).

init(_) ->
  {ok, #state{}}.

%% @private
%% @doc
%% Calls functions from {@link enb_message_parser}
%%
%% @see enb_message_parser
%% @end
-spec(handle_call(Message :: {parse | unparse, #message{} | string()}, _ :: {pid(), Tag}, State :: #state{}) ->
  {noreply, State :: #state{}}).

handle_call({parse, RawMessage}, _, State) ->
  {reply, enb_message_parser:parse(RawMessage), State};
handle_call({unparse, Message}, _, State) ->
  {reply, enb_message_parser:unparse(Message), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @hidden
handle_cast(_Msg, State) ->
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
