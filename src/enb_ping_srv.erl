-module(enb_ping_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("message.hrl").

-record(state, {nickname}).

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
    {ok, Nickname} = application:get_env(erninebot, nickname),
    gen_server:cast(enb_message_exchange_srv, {subscribe, self(), ping}),
    {ok, #state{nickname = Nickname}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({message, #message{command = ping, last_param = Server}}, State = #state{nickname = Nickname}) ->
    enb_message_exchange_srv:send_message(#message{command = pong, params = [Nickname], last_param = Server}),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
