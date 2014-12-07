-module(enb_message_exchange_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("message.hrl").

-record(state, {subscriptions, all}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, send_message/1, subscribe/1]).

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
    {ok, #state{subscriptions = maps:new(), all = []}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% Message for Pid to subscribe to Command messages
handle_cast({subscribe, Pid, Command}, State = #state{subscriptions = Subs}) ->
    Existing = maps:get(Command, Subs, []),
    {noreply, State#state{subscriptions = maps:put(Command, [Pid | Existing], Subs)}};
handle_cast({subscribe_to_all, Pid}, State = #state{all = All}) ->
    {noreply, State#state{all = [Pid | All]}};

%% Incoming message to be distributed to subscribed servers
handle_cast({message_in, RawMessage}, State = #state{subscriptions = Subs, all = All}) ->
    Message = #message{command = Command} = gen_server:call(enb_parser_srv, {parse, RawMessage}),
    lists:map(fun(Pid) -> gen_server:cast(Pid, {message, Message}) end, maps:get(Command, Subs, [])),
    lists:map(fun(Pid) -> gen_server:cast(Pid, {message, Message}) end, All),
    {noreply, State};

%% Sends message Message
handle_cast({message_out, Message}, State) ->
    RawMessage = gen_server:call(enb_parser_srv, {unparse, Message}),
    gen_server:cast(enb_io_srv, {send_message, RawMessage}),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send_message(Message) ->
    gen_server:cast(enb_message_exchange_srv, {message_out, Message}).

subscribe(Command) ->
    gen_server:cast(enb_message_exchange_srv, {subscribe, self(), Command}).
%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

