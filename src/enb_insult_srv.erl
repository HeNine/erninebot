-module(enb_insult_srv).

-behaviour(gen_server).

-include("message.hrl").
-include("dictionary.hrl").

%% API
-export([start_link/0, init_database/0, insert_file/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  init_database(),
  enb_message_exchange_srv:subscribe(bot_command),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({message, #message{last_param = MessageText, params = [Channel | _]}}, State) ->
  case re:run(MessageText, enb_basic_filters:bot_command_re() ++ "\s+please insult (.+)", [unicode, {capture, all, list}]) of
    {match, [_, User]} ->
      enb_message_exchange_srv:send_message(
        #message{
          command = privmsg,
          params = [Channel],
          % <user>, you <adjective> <noun> <verb>ing <noun> <verb>er.
          last_param = io_lib:format(
            "~s, you ~s ~s ~sing ~s ~ser",
            [User, random_adj(), random_noun(), random_verb(), random_noun(), random_verb()])});
    nomatch -> ok
  end,
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_database() ->
  mnesia:delete_table(noun),
  mnesia:delete_table(verb),
  mnesia:delete_table(adjective),
  mnesia:delete_table(adverb),

  {atomic, ok} = mnesia:create_table(noun, [
    {attributes, record_info(fields, noun)}
  ]),
  {atomic, ok} = mnesia:create_table(verb, [
    {attributes, record_info(fields, verb)}
  ]),
  {atomic, ok} = mnesia:create_table(adjective, [
    {attributes, record_info(fields, adjective)}
  ]),
  {atomic, ok} = mnesia:create_table(adverb, [
    {attributes, record_info(fields, adverb)}
  ]),

  {ok, NounFile} = file:open(code:priv_dir(erninebot) ++ "/data_clip.noun", read),
  {atomic, ok} = mnesia:transaction(fun enb_insult_srv:insert_file/3, [NounFile, noun, 1]),

  {ok, VerbFile} = file:open(code:priv_dir(erninebot) ++ "/data_clip.verb", read),
  {atomic, ok} = mnesia:transaction(fun enb_insult_srv:insert_file/3, [VerbFile, verb, 1]),

  {ok, AdjectiveFile} = file:open(code:priv_dir(erninebot) ++ "/data_clip.adj", read),
  {atomic, ok} = mnesia:transaction(fun enb_insult_srv:insert_file/3, [AdjectiveFile, adjective, 1]),

  {ok, AdverbFile} = file:open(code:priv_dir(erninebot) ++ "/data_clip.adv", read),
  {atomic, ok} = mnesia:transaction(fun enb_insult_srv:insert_file/3, [AdverbFile, adverb, 1]).

insert_file(File, Table, Id) ->
  case file:read_line(File) of
    {ok, "\n"} -> insert_file(File, Table, Id);
    {ok, Word} ->
      mnesia:write({Table, Id, re:replace(Word, "\n", "", [{return, list}])}),
      insert_file(File, Table, Id + 1);
    eof -> ok
  end.

random_adj() ->
  Keys = mnesia:dirty_all_keys(adjective),
  Key = lists:nth(random:uniform(length(Keys)), Keys),
  [#adjective{word = Word}] = mnesia:dirty_read(adjective, Key),
  Word.

random_verb() ->
  Keys = mnesia:dirty_all_keys(verb),
  Key = lists:nth(random:uniform(length(Keys)), Keys),
  [#verb{word = Word}] = mnesia:dirty_read(verb, Key),
  Word.

random_noun() ->
  Keys = mnesia:dirty_all_keys(noun),
  Key = lists:nth(random:uniform(length(Keys)), Keys),
  [#noun{word = Word}] = mnesia:dirty_read(noun, Key),
  Word.
