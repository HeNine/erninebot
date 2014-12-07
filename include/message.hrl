-record(message, {
  prefix,
  command,
  params = [],
  last_param = [],
  original
}).

-record(userspec, {
  nickname,
  user,
  host
}).

-record(serverspec, {
  servername
}).