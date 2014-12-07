-module(enb_channel_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {channels = [], joined = []}).

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
    enb_message_exchange_srv:subscribe(rpl_welcome),
    enb_message_exchange_srv:subscribe(rpl_topic),

    enb_message_exchange_srv:subscribe(err_needmoreparams),
    enb_message_exchange_srv:subscribe(err_bannedfromchan),
    enb_message_exchange_srv:subscribe(err_inviteonlychan),
    enb_message_exchange_srv:subscribe(err_badchannelkey),
    enb_message_exchange_srv:subscribe(err_channelisfull),
    enb_message_exchange_srv:subscribe(err_badchanmask),
    enb_message_exchange_srv:subscribe(err_nosuchchannel),
    enb_message_exchange_srv:subscribe(err_toomanychannels),
    enb_message_exchange_srv:subscribe(err_toomanytargets),
    enb_message_exchange_srv:subscribe(err_unavailresource),

    {ok, Channels} = application:get_env(erninebot, channels),

    {ok, #state{channels = Channels}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({message, #message{command = rpl_welcome}}, State = #state{channels = Channels}) ->
    enb_message_exchange_srv:send_message(#message{command = join, params = [string:join(Channels, ",")]}),
    {noreply, State#state{joined = Channels}};
handle_cast(_, State) ->
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

