-record(message,{
  prefix,
  command,
  params = []
}).

-record(userspec,{
  nickname,
  user,
  host
}).

-record(serverspec,{
  servername
}).