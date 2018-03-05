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

-define(INBOX_TABLE, <<"loveos_inbox_v1">>).

-export([start/2,
   stop/1,
   reload/3,
   depends/2,
   get_inbox/2,
   ensure_sql/2,
   process_iq/1,
   user_send_packet/1,
   user_receive_packet/1
]).

%%% Module start/stop

start(Host, Opts) ->
  IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1, one_queue),             %% IQDisc is required for module to IQ handler to work
  gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_INBOX, ?MODULE, process_iq, IQDisc),
  ejabberd_hooks:add(user_send_packet, Host, ?MODULE, user_send_packet, 85),
  ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, user_receive_packet, 85),
  xmpp:register_codec(xmpp_loveos_inbox),
  ensure_sql(Host).

stop(Host) ->
  gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_INBOX),
  gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_INBOX),
  ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, user_send_packet, 85),
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
  [<<" select i.id, i.peer_user, i.peer_server, i.message, i.direction, i.read, floor(extract(epoch from i.timestamp) * 1000) as timestamp, p.display_name, p.profile_picture from loveos_inbox_v1 i ">>,
   <<" join loveos_profiles_v1 p on p.user_id = i.peer_user left join loveos_excluded_v1 e on i.username=e.user_id ">>,
   <<" where (e.excluded_users::jsonb ? peer_user) != true and username = '">>, User, <<"'">>].

make_inbox_element(MessageId, Jid, Message, Direction, ReadStatus, Timestamp, DisplayName, ProfilePicture) ->
    #inbox_item{ 
      user = #inbox_user{
        jid = Jid,
        display_name = DisplayName,  
        picture_url = ProfilePicture
      },
      last_message = #inbox_last_message{
        id = MessageId,
        text = Message,
        direction = Direction,
        timestamp = Timestamp,
        read = ReadStatus
      }
    }.

get_inbox(Host, User) ->
  Query = query_messages(User),
  QueryResult = ejabberd_sql:sql_query(Host, Query),
  case QueryResult of
    {selected, _Columns, Rows} when is_list(Rows) ->
      [ make_inbox_element(
        MessageId, 
        jid:make(PeerUser, PeerServer), 
        Message, 
        case Direction of <<"I">> -> <<"incoming">>; <<"O">> -> <<"outgoing">> end, 
        case ReadStatus of <<"t">> -> <<"true">>; <<"f">> -> <<"false">> end, 
        Timestamp, 
        DisplayName, 
        ProfilePicture
      ) || [MessageId, PeerUser, PeerServer, Message, Direction, ReadStatus, Timestamp, DisplayName, ProfilePicture ] <- Rows ];
      % ?INFO_MSG("QR: ~p ", [Elements]),
      % Elements;
    _ -> []
  end.

process_iq(#iq{type = get, from = #jid{luser = User, lserver = Host}} = IQ) ->
    xmpp:make_iq_result(IQ, #inbox_query{ items = get_inbox(Host, User)}).
    

process_message(_, _, _, <<"">>, _) -> 
  ok;
process_message(Id, #jid{ luser = CurrentUser, lserver = CurrentServer } = _Current, #jid{ luser = PeerUser, lserver = PeerServer } = _Peer, Message, Direction) ->
  % Peer = jlib:jid_to_string(jlib:jid_remove_resource(PeerJid)),
  Query = [
    <<"insert into ">>, ?INBOX_TABLE, <<" (id, username, peer_user, peer_server, message, direction, read) ">>,
    <<"values ('">>, Id, <<"','">>, CurrentUser, <<"','">>, PeerUser, <<"','">>, PeerServer, <<"','">>, Message, <<"','">>, Direction, <<"', false)">>,
    <<" on conflict on constraint c_loveos_inbox_v1_user_peer do update set ">>,
    <<"id = '">>, Id, <<"', ">>,
    <<"message = '">>, Message, <<"', ">>,
    <<"direction = '">>, Direction, <<"', ">>,
    <<"read = false">>
  ],

  case ejabberd_sql:sql_query(CurrentServer, Query) of
    {updated, _} -> ok;
    Err -> 
      ?INFO_MSG("Error upserting: ~p", [Err]),
      Err
  end.

process_update_read(#receipt_response{id = Id}, #jid{ lserver = Server }) -> 
  Query = [
    <<"update ">>, ?INBOX_TABLE, <<" set read = true where id = '">>,
    Id,
    <<"'">>
  ],
  case ejabberd_sql:sql_query(Server, Query) of
    {updated, _} -> ok;
    Err -> 
      ?INFO_MSG("Error upserting: ~p", [Err]),
      Err
  end.

process_message(#message{id = Id, from = From, to = To, body = Body } = Message, Direction) ->
  case xmpp:has_subtag(Message, #receipt_response{}) of
    true ->
      process_update_read(xmpp:get_subtag(Message, #receipt_response{}), From);
    false -> 
      BodyText = xmpp:get_text(Body),
      case Direction of 
        <<"incoming">> -> process_message(Id, To, From, BodyText, <<"I">>);
        <<"outgoing">> -> process_message(Id, From, To, BodyText, <<"O">>)
      end
  end.

process_packet({ #message{} = Message, _C2SState} = Input, Direction) ->
  process_message(Message, Direction),
  Input;
process_packet(Input, _Direction) ->
  Input.

user_send_packet(Input) ->
  process_packet(Input, <<"outgoing">>).
user_receive_packet(Input) ->
  process_packet(Input, <<"incoming">>).
