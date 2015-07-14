%% @doc
%% Echo server is a service that echoes part of a mesage back to the channel on command.
%%
%% @since 1.0
%% @end
-module(enb_echo_srv).
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
%% Initializes the server.
%%
%% Subscribes to 'bot_command' messages.
%%
%% @end
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} | {stop, Reason :: term()} | ignore).
init([]) ->
  enb_message_exchange_srv:subscribe(bot_command),
  {ok, #state{}}.

%% @hidden
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @private
%% @doc
%% Echoes part of the message back to the chennel.
%%
%% On getting a 'bot_command' message that starts with "say " echoes the rest of the message back to the channel.
%%
%% @end
-spec(handle_cast(Message :: {message, #message{}}, State :: #state{}) ->
  {noreply, NewState :: #state{}}).
handle_cast({message, #message{params = [Channel | _], last_param = MessageText}}, State) ->
  case re:run(MessageText, enb_basic_filters:bot_command_re() ++ "\s+say (.+)", [unicode, {capture, all, list}]) of
    {match, [_, Say]} ->
      enb_message_exchange_srv:send_message(#message{command = privmsg, params = [Channel], last_param = Say});
    nomatch -> ok
  end,
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

%% @hidden
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%% @hidden
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%% @hidden
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal Function Definitions
