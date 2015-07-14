%% @doc
%% A service that insults an enitity. (Badly.)
%%
%% The insult server is a service server that responds to a bot command "please insult &lt;string&gt;" with
%% "&lt;string&gt;, you &lt;adjective&gt; &lt;noun&gt; &lt;verb&gt;ing &lt;noun&gt; &lt;verb&gt;er."
%%
%% @since 1.0
%% @end
-module(enb_insult_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {}).

-include("message.hrl").
-include("dictionary.hrl").

%% API Function Exports
-export([
  start_link/0,
  insert_file/3]).

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
%% Initializes the server
%%
%% Loads the word database
%%
%% @todo: Switch to rand
%% @since 1.0
%% @end
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).

init(_) ->
  random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
  init_database(),
  enb_message_exchange_srv:subscribe(bot_command),
  {ok, #state{}}.

%% @hidden
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%% @private
%% @doc
%% Replies to 'please insult' commands
%%
%% On receiving a "&lt;command&gt;>, please insult &lt;string&gt;" replies with "please insult &lt;string&gt;" with "&lt;string&gt;,
%% you &lt;adjective&gt; &lt;noun&gt; &lt;verb&gt;ing &lt;noun&gt; &lt;verb&gt;er."
%%
%% @end
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}}).
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
