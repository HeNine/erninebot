%%%-------------------------------------------------------------------
%%% @author henine
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2015 8:47 AM
%%%-------------------------------------------------------------------
-module(enb_basic_filters).

-include("message.hrl").

%% API
-export([bot_command_filter/1, bot_command_re/0, all_filter/1, command_filter/2]).

all_filter(_Message) ->
  true.

command_filter(#message{command = MessageCommand}, Command) ->
  MessageCommand == Command.

bot_command_filter(#message{command = Command, last_param = MessageText}) when Command == privmsg ->
  case re:run(MessageText, bot_command_re()) of
    {match, _} -> true;
    nomatch -> false
  end;
bot_command_filter(_) -> false.

bot_command_re() -> "^[Ee]r[Nn]ine(?:[Bb]ot)?,".
