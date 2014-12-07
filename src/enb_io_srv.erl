-module(enb_io_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {server, port, nickname, socket, state = disconnected}).

-include("message.hrl").

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
    gen_server:cast(enb_message_exchange_srv, {subscribe, self(), rpl_welcome}),
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
    gen_tcp:send(State#state.socket, enb_message_parser:unparse(pass, ["nopass"], [])),
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

%% Server welecomes us
handle_cast({message, #message{command = rpl_welcome}}, State) ->
    {noreply, State#state{state = handshake_complete}};

%% Command to send a message
handle_cast({send_message, RawMessage}, State = #state{state = handshake_complete}) ->
    gen_tcp:send(
        State#state.socket,
        RawMessage
    ),
    {noreply, State}.

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

terminate(_Reason, _State = #state{socket = Socket}) ->
    gen_tcp:send(Socket, <<"QUIT :Who can say where the road goes...">>),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

