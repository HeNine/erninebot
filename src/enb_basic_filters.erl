%% @doc
%% This module contains functions used to filter messages. Every function's first argument must be #message{}.
%% Other arguments are appended in an @{M, F, A@} style call.
%%
%% @since 1.0
%% @end
-module(enb_basic_filters).

-include("message.hrl").

%% Filter functions
-export([
  bot_command_filter/1,
  all_filter/1,
  command_filter/2]).

%% Auxiliary functions
-export([bot_command_re/0]).

%% @doc
%% Returns true for any message, therefore allowing it through the filter. Used to subscribe to all messages.
%%
%% @since 1.0
%% @end
-spec(all_filter(_Message :: #message{}) ->
  boolean()).

all_filter(_Message) ->
  true.

%% @doc
%% Used to filter a specific IRC command. The command should be in atom form compatible with {@link enb_message_parser}.
%%
%% @since 1.0
%% @end
-spec(command_filter(#message{}, Command :: atom()) ->
  boolean()).

command_filter(#message{command = MessageCommand}, Command) ->
  MessageCommand == Command.

%% @doc
%% Allows through messages that are a command for the bot.
%%
%% @since 1.0
%% @end
-spec(bot_command_filter(#message{}) ->
  boolean()).

bot_command_filter(#message{command = Command, last_param = MessageText}) when Command == privmsg ->
  case re:run(MessageText, bot_command_re()) of
    {match, _} -> true;
    nomatch -> false
  end;
bot_command_filter(_) -> false.

%% @doc
%% @private
%% Returns a regular expression that matches the beginning of the line.
%%
%% @since 1.0
%% @end
-spec(bot_command_re() ->
  string()).

bot_command_re() -> "^[Ee]r[Nn]ine(?:[Bb]ot)?,".
