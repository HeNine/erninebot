-module(erninebot_io_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {server, port, nickname, socket, state = disconnected}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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
  {ok, Server} = application:get_env(erninebot, server),
  {ok, Port} = application:get_env(erninebot, port),
  {ok, Nickname} = application:get_env(erninebot, nickname),
  gen_server:cast(self(), {connect}),
  {ok, #state{server = Server, port = Port, nickname = Nickname}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% Initiate TCP connection
handle_cast({connect}, State = #state{state = disconnected}) ->
  {ok, Socket} = gen_tcp:connect(
    State#state.server,
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
  io:format("~s~n", ["Pass sent"]),
  gen_tcp:send(State#state.socket, <<"PASS nopass", $\r, $\n>>),
  gen_server:cast(self(), {nick}),
  {noreply, State#state{state = pass_sent}};

%% Send nick message
handle_cast({nick}, State = #state{state = pass_sent}) ->
  io:format("~s~n", ["Nick sent"]),
  BinaryNick = list_to_binary(State#state.nickname),
  gen_tcp:send(State#state.socket, <<"NICK ", BinaryNick/binary, $\r, $\n>>),
  gen_server:cast(self(), {user}),
  {noreply, State#state{state = nick_sent}};

%% Send user message
handle_cast({user}, State = #state{state = nick_sent}) ->
  io:format("~s~n", ["User sent"]),
  BinaryNick = list_to_binary(State#state.nickname),
  gen_tcp:send(State#state.socket, <<"USER ", BinaryNick/binary, " 8 * :Ernine Shadowrift", $\r, $\n>>),
  {noreply, State#state{state = user_sent}};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
  io:format("~s", [Data]),
  io:format("~w~n", [gen_server:call(erninebot_parser_srv, {parse, binary:bin_to_list(Data)})]),
  {noreply, State};
handle_info({tcp_closed, Socket}, State) ->
  {noreply, State};
handle_info({tcp_error, Socket, Reason}, State) ->
  io:write(Reason),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

