-module(erninebot_message_parser).
-include("message.hrl").

%% API
-export([parse/1]).

parse(Message) ->
  [Body | LastParam] = re:split(Message, " :", [{return, list}]),
  SplitMessage = re:split(Body, " ", [{return, list}]) ++ LastParam,
  parser(SplitMessage, {start, #message{}}).

%% parser(Input,{State,Message})
parser([In | Input], {start, _}) ->
  case In of
    [C | Prefix] when C == $: ->
      parser([Prefix | Input], {prefix, #message{}});
    In ->
      case re:run(In, "[a-zA-Z]+|[0-9]{3}", [{capture, none}]) of
        match -> parser([In | Input], {command, #message{}})
      end
  end;

parser([In | Input], {prefix, _}) ->
  case re:run(In,
    "^([0-9a-zA-Z][0-9A-Za-z-]*[0-9a-zA-Z]*(?:\\.[0-9a-zA-Z][0-9A-Za-z-]*[0-9a-zA-Z]*)*)$",
    [{capture, all_but_first, list}])
  of
    {match, [Servername]} ->
      parser(Input, {command, #message{prefix = #serverspec{servername = Servername}}});
    nomatch ->
      case re:run(In,
        "^([a-zA-Z\\x5B-\\x60\\x7B-\\x7D][0-9a-zA-Z\\x5B-\\x60\\x7B-\\x7D-]*)" ++
          "(?:(?:!([\\x01-\\x09\\x0B-\\x0C\\x0E-\\x0F\\x21-\\x3F\\x41-\\xFF]+))?" ++
          "@([0-9a-zA-Z][0-9A-Za-z-]*[0-9a-zA-Z]*(?:\\.[0-9a-zA-Z][0-9A-Za-z-]*[0-9a-zA-Z]*)*|" ++
          "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}|" ++
          %% Place for IPv6 support *ugh*
          "))?$",
        [{capture, all_but_first, list}]
      )
      of
        {match, [Nickname, User, Host]} ->
          parser(Input, {command, #message{prefix = #userspec{nickname = Nickname, user = User, host = Host}}})
      end
  end;

parser([In | Input], {command, Message}) ->
  case re:run(In, "^([a-zA-Z]+|[0-9]{3})$", [{capture, all_but_first, list}]) of
    {match, [Command]} -> parser(Input, {parameters, Message#message{command = Command}})
  end;

parser([], {parameters, Message}) -> Message;
parser([In], {parameters, Message = #message{params = Params}}) ->
  case re:run(
    In,
    "^([\\x01-\\x09\\x0B-\\x0C\\x0E-\\x0F\\x21-\\x39\\x3B-\\xFF: ]*)\\r\\n$",
    [{capture, all_but_first, list}])
  of
    {match, [Parameter]} -> parser([], {parameters, Message#message{params = Params ++ [Parameter]}})
  end;
parser([In | Input], {parameters, Message = #message{params = Params}}) ->
  case re:run(
    In,
    "^([\\x01-\\x09\\x0B-\\x0C\\x0E-\\x0F\\x21-\\x39\\x3B-\\xFF]" ++
    "[\\x01-\\x09\\x0B-\\x0C\\x0E-\\x0F\\x21-\\x39\\x3B-\\xFF:]*)$",
    [{capture, all_but_first, list}])
  of
    {match, [Parameter]} -> parser(Input, {parameters, Message#message{params = Params ++ [Parameter]}})
  end.

command_to_atom(Command)->
  case string:to_upper(Command) of
    "PASS"->pass;
    "NICK" -> nick;
    "USER" -> user;
    "OPER"->oper;
    "MODE"->mode;
    "SERVICE"->service;
    "QUIT"->quit;


  end.

atom_to_command(Atom)->
  case Atom of

  end
  .