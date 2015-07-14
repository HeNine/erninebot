%% @doc
%% The connection server.
%%
%% IO server takes care of managing the TCP connection to the server.
%%
%% @since 1.0
%% @end
-module(enb_io_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {server, port, nickname, socket, state = disconnected}).

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
%% Server initialization.
%%
%% Gets the server to connect to, the port and bot's nickname from the application configuration and saves them in the
%% state. Then the server subscribes to the welcome message and tells the state machine to connect.
%%
%% @end
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} | {stop, Reason :: term()} | ignore).

init(_Args) ->
  {ok, Server} = application:get_env(erninebot, server),
  {ok, Port} = application:get_env(erninebot, port),
  {ok, Nickname} = application:get_env(erninebot, nickname),
  gen_server:cast(enb_message_exchange_srv, {subscribe, self(), rpl_welcome}),
  gen_server:cast(self(), {connect}),
  {ok, #state{server = Server, port = Port, nickname = Nickname}}.

%% @hidden
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @private
%% @doc
%% The server uses a series of messages to steer a state machine to establish the connection and authenticate with the
%% IRC server.
%%
%% @end
%% Initiate TCP connection
handle_cast({connect}, State = #state{state = disconnected}) ->
  {ok, Socket} = gen_tcp:connect(
    State#state.server,
    %% We want the data in binary form and pre-split by lines (new line terminates IRC messages)
    State#state.port,
    [
      binary,
      {packet, line}
    ]
  ),
  gen_server:cast(self(), {pass}),
  {noreply, State#state{socket = Socket, state = connected}};

%% Send pass message
handle_cast({pass}, State = #state{state = connected}) ->
  gen_tcp:send(
    State#state.socket,
    enb_parser_srv:unparse(#message{command = pass, params = ["nopass"], last_param = []})),
  gen_server:cast(self(), {nick}),
  {noreply, State#state{state = pass_sent}};

%% Send nick message
handle_cast({nick}, State = #state{state = pass_sent}) ->
  gen_tcp:send(State#state.socket, enb_message_parser:unparse(nick, [State#state.nickname], [])),
  gen_server:cast(self(), {user}),
  {noreply, State#state{state = nick_sent}};

%% Send user message
handle_cast({user}, State = #state{state = nick_sent}) ->
  gen_tcp:send(
    State#state.socket,
    enb_message_parser:unparse(
      user,
      [
        State#state.nickname,
        "8",
        "*"
      ],
      "Ernine Shadowrift"
    )
  ),
  {noreply, State#state{state = user_sent}};

%% Server welcomes us
handle_cast({message, #message{command = rpl_welcome}}, State) ->
  {noreply, State#state{state = handshake_complete}};

%% Command to send a message
handle_cast({send_message, RawMessage}, State = #state{state = handshake_complete}) ->
  gen_tcp:send(
    State#state.socket,
    RawMessage
  ),
  {noreply, State}.

%% @private
%% @doc
%% gen_tcp passes data as regular messages. Here we decode it and send it to the exchange.
%%
%% @end
handle_info({tcp, _Socket, Data}, State) ->
  gen_server:cast(enb_message_exchange_srv, {message_in, binary_to_list(Data)}),
  {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
  {noreply, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
  io:write(Reason),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
%% @doc
%% If the server terminates it sends a QUIT message to the IRC server.
%%
%% @end
terminate(_Reason, _State = #state{socket = Socket}) ->
  gen_tcp:send(Socket, <<"QUIT :Who can say where the road goes...">>),
  ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal Function Definitions
