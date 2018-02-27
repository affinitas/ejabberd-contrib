%%%-------------------------------------------------------------------
%%% @author Igor Dultsev
%%% ©copyright (C) 2017-2018, Spark Networks Services, GmbH
%%% @doc
%%% Returns inbox to users
%%% @end
%%%-------------------------------------------------------------------

-module(mod_loveos_inbox).
-author("Igor Dultsev").

-behavior(gen_mod).
-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_commands.hrl").
-include("ejabberd_sql_pt.hrl").
-include("mod_mam.hrl").
-include("xmpp_loveos_inbox.hrl").

-export([start/2,
   stop/1,
   reload/3,
   depends/2,
   get_inbox/2,
   ensure_sql/2,
   process_iq/1
]).

%%% Module start/stop

start(Host, Opts) ->
  IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1, one_queue),             %% IQDisc is required for module to IQ handler to work
  gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_INBOX, ?MODULE, process_iq, IQDisc),
  xmpp:register_codec(xmpp_loveos_inbox),
  ensure_sql(Host).

stop(Host) ->
  gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_INBOX),
  gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_INBOX),
  xmpp:unregister_codec(xmpp_loveos_inbox),
  ok.

depends(_Host, _Opts) ->
    [].

reload(Host, NewOpts, OldOpts) ->
    xmpp:register_codec(inbox_query),
    NewMod = gen_mod:db_mod(Host, NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(Host, OldOpts, ?MODULE),
    if NewMod /= OldMod ->
      NewMod:init(Host, NewOpts);
        true ->
      ok
    end.

% This module can only be run on SQL-type DBs

ensure_sql(Host) -> ensure_sql(Host, gen_mod:db_type(Host, mod_mam)).
ensure_sql(_Host, sql) -> ok;
ensure_sql(_Host, DbType) -> erlang:error(iolist_to_binary(io_lib:format("Unsupported backend: ~p", [DbType]))).


% Process the following iq: <iq from="jid@server" type="get"><query xmlns="jabber:iq:inbox" /></iq>


query_messages(User) ->
  [<<"select distinct on (bare_peer) bare_peer, txt, timestamp FROM archive WHERE username='">>, User, <<"'order by bare_peer, created_at desc">>].

make_inbox_element(Jid, Message, Timestamp) ->
    Converted = jlib:string_to_jid(Jid),
    ?INFO_MSG("JID: ~p ~p ", [Jid, Converted]),
    #inbox_item{ 
      jid = Converted,
      name = <<"Unknown name">>,
      photo = <<"https://picture-storage.beta.loveos.io/pictures/chattery/1aee6403-5336-4091-9567-f1a9186dbfcd/profile/8b3f58bb-cf75-416d-8d9c-fc658dc2c604.jpeg">>,
      lastmsg = Message,
      messaged_at = Timestamp,
      read = <<"yes">>
    }.

get_inbox(Host, User) ->
  Query = query_messages(User),
  QueryResult = ejabberd_sql:sql_query(Host, Query),
  case QueryResult of
    {selected, _Columns, Rows} when is_list(Rows) ->
      [ make_inbox_element(Jid, Message, Timestamp) || [Jid, Message, Timestamp] <- Rows];
      % ?INFO_MSG("QR: ~p ", [Elements]),
      % Elements;
    _ -> []
  end.


process_iq(#iq{type = get, from = #jid{luser = User, lserver = Host}} = IQ) ->
  xmpp:make_iq_result(IQ, #inbox_query{ items = get_inbox(Host, User)}).
