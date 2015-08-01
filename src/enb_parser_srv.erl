%% @doc
%% Message persing server
%%
%% To ensure that the whole bot doesn't crash if a message fails to parse, the parsing functionality is available in a
%% server.
%%
%% @since 1.0
%% @end
-module(enb_parser_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {}).

-include("message.hrl").

%% API Function Exports
-export([
  start_link/0,
  unparse/1,
  send_message/1,
  receive_message/1]).

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
%% Utility function for message unparsing
%%
%% @since 1.0
%% @end
-spec(unparse(Message :: #message{}) ->
  binary()).

unparse(Message) ->
  gen_server:call(?SERVER, {unparse, Message}).

%% @doc
%% Send a message to IRC server
%%
%% @since 1.0
%% @end
-spec(send_message(Message :: #message{}) ->
  ok).

send_message(Message) ->
  gen_server:cast(?SERVER, {send_message, Message}).

%% @doc
%% Receive a message from IRC server
%%
%% @since 1.0
%% @end
-spec(receive_message(RawMessage :: string()) ->
  ok).

receive_message(RawMessage) ->
  gen_server:cast(?SERVER, {receive_message, RawMessage}).

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
%% Server may crash.
%%
%% @see enb_message_parser
%% @end
-spec(handle_call(Message :: {parse | unparse, #message{} | string()}, _ :: {pid(), _}, State :: #state{}) ->
  {noreply, State :: #state{}}).

handle_call({parse, RawMessage}, _, State) ->
  {reply, enb_message_parser:parse(RawMessage), State};
handle_call({unparse, Message}, _, State) ->
  {reply, enb_message_parser:unparse(Message), State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @private
%% @doc
%% Calls functions from {@link enb_message_parser}
%%
%% @see enb_message_parser
%% @end
-spec(handle_cast(Message :: {message, #message{}}, State :: #state{}) ->
  {noreply, State :: #state{}}).

%% Incoming message from enb_io_serv
handle_cast({receive_message, RawMessage}, State) ->
  gen_server:cast(enb_message_exchange_srv, {message_in, enb_message_parser:parse(RawMessage)}),
  {noreply, State};

%% Outgoing message from enb_message_exchange_srv
handle_cast({send_message, Message}, State) ->
  gen_server:cast(enb_io_srv, {send_message, enb_message_parser:unparse(Message)}),
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
