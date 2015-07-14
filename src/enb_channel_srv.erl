%% @doc
%% The channel server takes care of joining and leaving channels.
%%
%% @since 1.0
%% @end
-module(enb_channel_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {channels = [], joined = []}).

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
%% Server initialization. Takes care of subscribing to relevant messages.
%%
%% @end
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} | {stop, Reason :: term()} | ignore).

init(_Args) ->
  enb_message_exchange_srv:subscribe(join),

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

%% @hidden
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @private
%% @doc
%% Handles incoming messages.
%%
%% Handles 'rpl_welcome' message. On joining the server, joins all channels defined in the settings.
%%
%% Handles 'join' command. Joins the channel as instructed by a 'join' command.
%% @end
-spec(handle_cast(Message :: {message, #message{}}, State :: #state{}) ->
  {noreply, State :: #state{}}).

handle_cast({message, #message{command = rpl_welcome}}, State = #state{channels = Channels}) ->
  case Channels of
    [] -> ok;
    _ -> enb_message_exchange_srv:send_message(#message{command = join, params = [string:join(Channels, ",")]})
  end,
  {noreply, State#state{joined = Channels}};

handle_cast({message, #message{command = join, params = [ChannelList | _]}}, State = #state{channels = Channels}) ->
  JoinChannels = re:split(ChannelList, ","),
  {noreply, State#state{joined = JoinChannels ++ Channels}};

handle_cast(_, State) ->
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
