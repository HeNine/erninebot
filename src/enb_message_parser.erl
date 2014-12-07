-module(enb_message_parser).
-include("message.hrl").

%% API
-export([parse/1, unparse/3, unparse/1]).

parse(Message) ->
    [Body | LastParam] = re:split(Message, " :", [{return, list}]),
    SplitMessage = re:split(Body, " ", [{return, list}]),
    parser(SplitMessage, {start, #message{last_param = LastParam, original = Message}}).

unparse(#message{command = Command, params = Params, last_param = Last}) ->
    unparse(Command, Params, Last).
unparse(Command, Parameters, LastParameter) ->
    CommandBinary = list_to_binary(atom_to_command(Command)),
    JoinedBinary = list_to_binary(string:join(Parameters, " ")),
    LastBinary = list_to_binary(LastParameter),
    case {Parameters, LastParameter} of
        {[], []} -> <<CommandBinary/binary, $\r, $\n>>;
        {[], _} -> <<CommandBinary/binary, " :", LastBinary/binary, $\r, $\n>>;
        {_, []} -> <<CommandBinary/binary, " ", JoinedBinary/binary, $\r, $\n>>;
        _ -> <<CommandBinary/binary, " ", JoinedBinary/binary, " :", LastBinary/binary, $\r, $\n>>
    end.

%% parser(Input,{State,Message})
parser([In | Input], {start, Message}) ->
    case In of
        [C | Prefix] when C == $: ->
            parser([Prefix | Input], {prefix, Message});
        In ->
            case re:run(In, "[a-zA-Z]+|[0-9]{3}", [{capture, none}]) of
                match -> parser([In | Input], {command, Message})
            end
    end;

parser([In | Input], {prefix, Message}) ->
    case re:run(In,
        "^([0-9a-zA-Z][0-9A-Za-z-]*[0-9a-zA-Z]*(?:\\.[0-9a-zA-Z][0-9A-Za-z-]*[0-9a-zA-Z]*)*)$",
        [{capture, all_but_first, list}])
    of
        {match, [Servername]} ->
            parser(Input, {command, Message#message{prefix = #serverspec{servername = Servername}}});
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
                    parser(Input, {command, Message#message{prefix = #userspec{nickname = Nickname, user = User, host =
                    Host}}})
            end
    end;

parser([In | Input], {command, Message}) ->
    case re:run(In, "^([a-zA-Z]+|[0-9]{3})$", [{capture, all_but_first, list}]) of
        {match, [Command]} -> parser(Input, {parameters, Message#message{command = command_to_atom(Command)}})
    end;

parser([], {parameters, Message}) -> Message;
%% parser([In], {parameters, Message = #message{params = Params}}) ->
%%   case re:run(
%%     In,
%%     "^([\\x01-\\x09\\x0B-\\x0C\\x0E-\\x0F\\x21-\\x39\\x3B-\\xFF: ]*)\\r\\n$",
%%     [{capture, all_but_first, list}])
%%   of
%%     {match, [Parameter]} -> parser([], {parameters, Message#message{last_param = Parameter}})
%%   end;
parser([In | Input], {parameters, Message = #message{params = Params}}) ->
    case re:run(
        In,
        "^([\\x01-\\x09\\x0B-\\x0C\\x0E-\\x0F\\x21-\\x39\\x3B-\\xFF]" ++
        "[\\x01-\\x09\\x0B-\\x0C\\x0E-\\x0F\\x21-\\x39\\x3B-\\xFF:]*)(?:\r\n)?$",
        [{capture, all_but_first, list}])
    of
        {match, [Parameter]} -> parser(Input, {parameters, Message#message{params = Params ++ [Parameter]}});
        nomatch -> io:format("~s~n", [In])
    end.

command_to_atom(Command) ->
    case string:to_upper(Command) of
%% Connection registration
        "PASS" -> pass;
        "NICK" -> nick;
        "USER" -> user;
        "OPER" -> oper;
        "MODE" -> mode;
        "SERVICE" -> service;
        "QUIT" -> quit;
        "SQUIT" -> squit;

%% Channel operations
        "JOIN" -> join;
        "PART" -> part;
        "TOPIC" -> topic;
        "NAMES" -> names;
        "LIST" -> list;
        "INVITE" -> invite;
        "KICK" -> kick;

%% Messages
        "PRIVMSG" -> privmsg;
        "NOTICE" -> notice;

%% Server queries and commands
        "MOTD" -> motd;
        "LUSERS" -> lusers;
        "VERSION" -> version;
        "STATS" -> stats;
        "LINKS" -> links;
        "TIME" -> time;
        "CONNECT" -> connect;
        "TRACE" -> trace;
        "ADMIN" -> admin;
        "INFO" -> info;

%% Service query and commands
        "SERVLIST" -> servlist;
        "SQUERY" -> squery;

%% User based queries
        "WHO" -> who;
        "WHOIS" -> whois;
        "WHOWAS" -> whowas;

%% Miscellaneous messages
        "KILL" -> kill;
        "PING" -> ping;
        "PONG" -> pong;
        "ERROR" -> error;

%% Optional
        "AWAY" -> away;
        "REHASH" -> rehash;
        "DIE" -> die;
        "RESTART" -> restart;
        "SUMMON" -> summon;
        "USERS" -> users;
        "WALLOPS" -> wallops;
        "USERHOST" -> userhost;
        "ISON" -> ison;
%%
%% Server responses
%%
        "001" -> rpl_welcome;
        "002" -> rpl_yourhost;
        "003" -> rpl_created;
        "004" -> rpl_myinfo;
        "005" -> rpl_isupport;
        "010" -> rpl_bounce;
        "015" -> rpl_map;
        "017" -> rpl_mapend;
        "018" -> rpl_mapstart;
        "020" -> rpl_hello;
        "042" -> rpl_yourid;
        "043" -> rpl_savenick;
        "200" -> rpl_tracelink;
        "201" -> rpl_traceconnecting;
        "202" -> rpl_tracehandshake;
        "203" -> rpl_traceunknown;
        "204" -> rpl_traceoperator;
        "205" -> rpl_traceuser;
        "206" -> rpl_traceserver;
        "207" -> rpl_traceservice;
        "208" -> rpl_tracenewtype;
        "209" -> rpl_traceclass;
        "210" -> rpl_tracereconnect;
        "211" -> rpl_statslinkinfo;
        "212" -> rpl_statscommands;
        "213" -> rpl_statscline;
        "214" -> rpl_statsnline;
        "215" -> rpl_statsiline;
        "216" -> rpl_statskline;
        "217" -> rpl_statsqline;
        "218" -> rpl_statsyline;
        "219" -> rpl_endofstats;
        "221" -> rpl_umodeis;
        "225" -> rpl_statsdline;
        "227" -> rpl_option;
        "228" -> rpl_endoptions;
        "231" -> rpl_serviceinfo;
        "232" -> rpl_endofservices;
        "233" -> rpl_service;
        "234" -> rpl_servlist;
        "235" -> rpl_servlistend;
        "240" -> rpl_statsvline;
        "241" -> rpl_statslline;
        "242" -> rpl_statsuptime;
        "243" -> rpl_statsonline;
        "244" -> rpl_statshline;
        "245" -> rpl_statssline;
        "246" -> rpl_statsping;
        "247" -> rpl_statsbline;
        "248" -> rpl_statsuline;
        "249" -> rpl_statsdebug;
        "250" -> rpl_statsdline;
        "251" -> rpl_luserclient;
        "252" -> rpl_luserop;
        "253" -> rpl_luserunknown;
        "254" -> rpl_luserchannels;
        "255" -> rpl_luserme;
        "256" -> rpl_adminme;
        "257" -> rpl_adminloc1;
        "258" -> rpl_adminloc2;
        "259" -> rpl_adminemail;
        "261" -> rpl_tracelog;
        "262" -> rpl_traceend;
        "263" -> rpl_tryagain;
        "265" -> rpl_localusers;
        "266" -> rpl_globalusers;
        "268" -> rpl_mode;
        "269" -> rpl_endmode;
        "271" -> rpl_sitelist;
        "272" -> rpl_endsitelist;
        "290" -> rpl_clientcapab;
        "292" -> rpl_noservicehost;
        "300" -> rpl_none;
        "301" -> rpl_away;
        "302" -> rpl_userhost;
        "303" -> rpl_ison;
        "304" -> rpl_away;
        "305" -> rpl_unaway;
        "306" -> rpl_noaway;
        "311" -> rpl_whoisuser;
        "312" -> rpl_whoisserver;
        "313" -> rpl_whoisoperator;
        "314" -> rpl_whowasuser;
        "315" -> rpl_endofwho;
        "316" -> rpl_whoischanop;
        "317" -> rpl_whoisidle;
        "318" -> rpl_endofwhois;
        "319" -> rpl_whoischannels;
        "320" -> rpl_whoisidentified;
        "321" -> rpl_liststart;
        "322" -> rpl_list;
        "323" -> rpl_listend;
        "324" -> rpl_channelmodeis;
        "325" -> rpl_uniqopis;
        "326" -> rpl_whoisoperprivs;
        "327" -> rpl_whoisrealhost;
        "329" -> rpl_creationtime;
        "331" -> rpl_notopic;
        "332" -> rpl_topic;
        "333" -> rpl_topicwhotime;
        "341" -> rpl_inviting;
        "342" -> rpl_summoning;
        "346" -> rpl_invitelist;
        "347" -> rpl_endofinvitelist;
        "348" -> rpl_exceptlist;
        "349" -> rpl_endofexceptlist;
        "351" -> rpl_version;
        "352" -> rpl_whoreply;
        "353" -> rpl_namreply;
        "361" -> rpl_killdone;
        "362" -> rpl_closing;
        "363" -> rpl_closeend;
        "366" -> rpl_endofnames;
        "364" -> rpl_links;
        "365" -> rpl_endoflinks;
        "367" -> rpl_banlist;
        "368" -> rpl_endofbanlist;
        "369" -> rpl_endofwhowas;
        "371" -> rpl_info;
        "372" -> rpl_motd;
        "373" -> rpl_infostart;
        "374" -> rpl_endofinfo;
        "375" -> rpl_motdstart;
        "376" -> rpl_endofmotd;
        "377" -> rpl_map;
        "378" -> rpl_endofmap;
        "379" -> rpl_forward;
        "381" -> rpl_youreoper;
        "382" -> rpl_rehashing;
        "383" -> rpl_yourservice;
        "384" -> rpl_myportis;
        "391" -> rpl_time;
        "392" -> rpl_usersstart;
        "393" -> rpl_users;
        "394" -> rpl_endofusers;
        "395" -> rpl_nousers;
        "399" -> rpl_message;
        "401" -> err_nosuchnick;
        "402" -> err_nosuchserver;
        "403" -> err_nosuchchannel;
        "404" -> err_cannotsendtochan;
        "405" -> err_toomanychannels;
        "406" -> err_wasnosuchnick;
        "407" -> err_toomanytargets;
        "408" -> err_nosuchservice;
        "409" -> err_noorigin;
        "410" -> 'err_services-offline';
        "411" -> err_norecipient;
        "412" -> err_notexttosend;
        "413" -> err_notoplevel;
        "414" -> err_wildtoplevel;
        "415" -> err_badmask;
        "421" -> err_unknowncommand;
        "422" -> err_nomotd;
        "423" -> err_noadmininfo;
        "424" -> err_fileerror;
        "431" -> err_nonicknamegiven;
        "432" -> err_erroneusnickname;
        "433" -> err_nicknameinuse;
        "436" -> err_nickcollision;
        "437" -> err_unavailresource;
        "438" -> err_bannickchange;
        "441" -> err_usernotinchannel;
        "442" -> err_notonchannel;
        "443" -> err_useronchannel;
        "444" -> err_nologin;
        "445" -> err_summondisabled;
        "446" -> err_userdisabled;
        "447" -> err_targetninvite;
        "448" -> err_sourceninvite;
        "451" -> err_notregistered;
        "461" -> err_needmoreparams;
        "462" -> err_alreadyregistered;
        "463" -> err_nopermforhost;
        "464" -> err_passwdmismatch;
        "465" -> err_yourebannedcreep;
        "466" -> err_youwillbebanned;
        "467" -> err_keyset;
        "471" -> err_channelisfull;
        "472" -> err_unknownmode;
        "473" -> err_inviteonlychan;
        "474" -> err_bannedfromchan;
        "475" -> err_badchannelkey;
        "476" -> err_badchanmask;
        "477" -> err_nochanmodes;
        "478" -> err_banlistfull;
        "479" -> err_badchanname;
        "480" -> err_throttled;
        "481" -> err_noprivileges;
        "482" -> err_chanoprivsneeded;
        "483" -> err_cantkillserver;
        "484" -> err_restricted;
        "485" -> err_uniqopprivsneeded;
        "486" -> err_restricted;
        "487" -> 'err_no-op-split';
        "488" -> 'err_need-umode';
        "491" -> err_nooperhost;
        "501" -> err_umodeunknownflag;
        "502" -> err_usersdontmatch;
        "503" -> err_ghostedclient;
        "505" -> 'err_blocking-notid';
        "511" -> err_sitelistfull;
        "512" -> err_maxmapnodes;
        "513" -> err_maxforwarding;
        "514" -> err_noforwarding;
        "515" -> err_nounidentified;
        "516" -> err_last_err_msg
    end.

atom_to_command(Atom) ->
    case Atom of
%% Connection registration
        pass -> "PASS";
        nick -> "NICK";
        user -> "USER";
        oper -> "OPER";
        mode -> "MODE";
        service -> "SERVICE";
        quit -> "QUIT";
        squit -> "SQUIT";

%% Channel operations
        join -> "JOIN";
        part -> "PART";
        topic -> "TOPIC";
        names -> "NAMES";
        list -> "LIST";
        invite -> "INVITE";
        kick -> "KICK";

%% Messages
        privmsg -> "PRIVMSG";
        notice -> "NOTICE";

%% Server queries and commands
        motd -> "MOTD";
        lusers -> "LUSERS";
        version -> "VERSION";
        stats -> "STATS";
        links -> "LINKS";
        time -> "TIME";
        connect -> "CONNECT";
        trace -> "TRACE";
        admin -> "ADMIN";
        info -> "INFO";

%% Service query and commands
        servlist -> "SERVLIST";
        squery -> "SQUERY";

%% User based queries
        who -> "WHO";
        whois -> "WHOIS";
        whowas -> "WHOWAS";

%% Miscellaneous messages
        kill -> "KILL";
        ping -> "PING";
        pong -> "PONG";
        error -> "ERROR";

%% Optional
        away -> "AWAY";
        rehash -> "REHASH";
        die -> "DIE";
        restart -> "RESTART";
        summon -> "SUMMON";
        users -> "USERS";
        wallops -> "WALLOPS";
        userhost -> "USERHOST";
        ison -> "ISON";

%%
%% Server responses
%%
        rpl_welcome -> "001";
        rpl_yourhost -> "002";
        rpl_created -> "003";
        rpl_myinfo -> "004";
        rpl_isupport -> "005";
        rpl_bounce -> "010";
        rpl_map -> "015";
        rpl_mapend -> "017";
        rpl_mapstart -> "018";
        rpl_hello -> "020";
        rpl_yourid -> "042";
        rpl_savenick -> "043";
        rpl_tracelink -> "200";
        rpl_traceconnecting -> "201";
        rpl_tracehandshake -> "202";
        rpl_traceunknown -> "203";
        rpl_traceoperator -> "204";
        rpl_traceuser -> "205";
        rpl_traceserver -> "206";
        rpl_traceservice -> "207";
        rpl_tracenewtype -> "208";
        rpl_traceclass -> "209";
        rpl_tracereconnect -> "210";
        rpl_statslinkinfo -> "211";
        rpl_statscommands -> "212";
        rpl_statscline -> "213";
        rpl_statsnline -> "214";
        rpl_statsiline -> "215";
        rpl_statskline -> "216";
        rpl_statsqline -> "217";
        rpl_statsyline -> "218";
        rpl_endofstats -> "219";
        rpl_umodeis -> "221";
        rpl_statsdline -> "225";
        rpl_option -> "227";
        rpl_endoptions -> "228";
        rpl_serviceinfo -> "231";
        rpl_endofservices -> "232";
        rpl_service -> "233";
        rpl_servlist -> "234";
        rpl_servlistend -> "235";
        rpl_statsvline -> "240";
        rpl_statslline -> "241";
        rpl_statsuptime -> "242";
        rpl_statsonline -> "243";
        rpl_statshline -> "244";
        rpl_statssline -> "245";
        rpl_statsping -> "246";
        rpl_statsbline -> "247";
        rpl_statsuline -> "248";
        rpl_statsdebug -> "249";
        rpl_luserclient -> "251";
        rpl_luserop -> "252";
        rpl_luserunknown -> "253";
        rpl_luserchannels -> "254";
        rpl_luserme -> "255";
        rpl_adminme -> "256";
        rpl_adminloc1 -> "257";
        rpl_adminloc2 -> "258";
        rpl_adminemail -> "259";
        rpl_tracelog -> "261";
        rpl_traceend -> "262";
        rpl_tryagain -> "263";
        rpl_localusers -> "265";
        rpl_globalusers -> "266";
        rpl_mode -> "268";
        rpl_endmode -> "269";
        rpl_sitelist -> "271";
        rpl_endsitelist -> "272";
        rpl_clientcapab -> "290";
        rpl_noservicehost -> "292";
        rpl_none -> "300";
        rpl_away -> "301";
        rpl_userhost -> "302";
        rpl_ison -> "303";
        rpl_noaway -> "306";
        rpl_whoisuser -> "311";
        rpl_whoisserver -> "312";
        rpl_whoisoperator -> "313";
        rpl_whowasuser -> "314";
        rpl_endofwho -> "315";
        rpl_whoischanop -> "316";
        rpl_whoisidle -> "317";
        rpl_endofwhois -> "318";
        rpl_whoischannels -> "319";
        rpl_whoisidentified -> "320";
        rpl_liststart -> "321";
        rpl_list -> "322";
        rpl_listend -> "323";
        rpl_channelmodeis -> "324";
        rpl_uniqopis -> "325";
        rpl_whoisoperprivs -> "326";
        rpl_whoisrealhost -> "327";
        rpl_creationtime -> "329";
        rpl_notopic -> "331";
        rpl_topic -> "332";
        rpl_topicwhotime -> "333";
        rpl_inviting -> "341";
        rpl_summoning -> "342";
        rpl_invitelist -> "346";
        rpl_endofinvitelist -> "347";
        rpl_exceptlist -> "348";
        rpl_endofexceptlist -> "349";
        rpl_version -> "351";
        rpl_whoreply -> "352";
        rpl_namreply -> "353";
        rpl_killdone -> "361";
        rpl_closing -> "362";
        rpl_closeend -> "363";
        rpl_endofnames -> "366";
        rpl_links -> "364";
        rpl_endoflinks -> "365";
        rpl_banlist -> "367";
        rpl_endofbanlist -> "368";
        rpl_endofwhowas -> "369";
        rpl_info -> "371";
        rpl_motd -> "372";
        rpl_infostart -> "373";
        rpl_endofinfo -> "374";
        rpl_motdstart -> "375";
        rpl_endofmotd -> "376";
        rpl_youreoper -> "381";
        rpl_rehashing -> "382";
        rpl_yourservice -> "383";
        rpl_myportis -> "384";
        rpl_time -> "391";
        rpl_usersstart -> "392";
        rpl_users -> "393";
        rpl_endofusers -> "394";
        rpl_nousers -> "395";
        rpl_message -> "399";
        err_nosuchnick -> "401";
        err_nosuchserver -> "402";
        err_nosuchchannel -> "403";
        err_cannotsendtochan -> "404";
        err_toomanychannels -> "405";
        err_wasnosuchnick -> "406";
        err_toomanytargets -> "407";
        err_nosuchservice -> "408";
        err_noorigin -> "409";
        'err_services-offline' -> "410";
        err_norecipient -> "411";
        err_notexttosend -> "412";
        err_notoplevel -> "413";
        err_wildtoplevel -> "414";
        err_badmask -> "415";
        err_unknowncommand -> "421";
        err_nomotd -> "422";
        err_noadmininfo -> "423";
        err_fileerror -> "424";
        err_nonicknamegiven -> "431";
        err_erroneusnickname -> "432";
        err_nicknameinuse -> "433";
        err_nickcollision -> "436";
        err_unavailresource -> "437";
        err_bannickchange -> "438";
        err_usernotinchannel -> "441";
        err_notonchannel -> "442";
        err_useronchannel -> "443";
        err_nologin -> "444";
        err_summondisabled -> "445";
        err_userdisabled -> "446";
        err_targetninvite -> "447";
        err_sourceninvite -> "448";
        err_notregistered -> "451";
        err_needmoreparams -> "461";
        err_alreadyregistered -> "462";
        err_nopermforhost -> "463";
        err_passwdmismatch -> "464";
        err_yourebannedcreep -> "465";
        err_youwillbebanned -> "466";
        err_keyset -> "467";
        err_channelisfull -> "471";
        err_unknownmode -> "472";
        err_inviteonlychan -> "473";
        err_bannedfromchan -> "474";
        err_badchannelkey -> "475";
        err_badchanmask -> "476";
        err_nochanmodes -> "477";
        err_banlistfull -> "478";
        err_badchanname -> "479";
        err_throttled -> "480";
        err_noprivileges -> "481";
        err_chanoprivsneeded -> "482";
        err_cantkillserver -> "483";
        err_restricted -> "484";
        err_uniqopprivsneeded -> "485";
        err_umodeunknownflag -> "501";
        err_usersdontmatch -> "502";
        err_ghostedclient -> "503";
        'err_blocking-notid' -> "505";
        err_sitelistfull -> "511";
        err_maxmapnodes -> "512";
        err_maxforwarding -> "513";
        err_noforwarding -> "514";
        err_nounidentified -> "515";
        err_last_err_msg -> "516"
    end.