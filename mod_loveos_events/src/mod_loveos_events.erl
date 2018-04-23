%%%-------------------------------------------------------------------
%%% @author Igor Dultsev
%%% ©copyright (C) 2017-2018, Affinitas.de
%%% @doc
%%%
%%%
%%% @end
%%% Create: 10th of April 2018
%%%-------------------------------------------------------------------

-module(mod_loveos_events).
-author("Igor Dultsev").

-behavior(gen_mod).
-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_commands.hrl").
-include("ejabberd_sql_pt.hrl").
-include("mod_mam.hrl").

-record(event_msg, {
  id = <<>> :: binary(),
  from :: jid:jid(),
  to :: jid:jid()
}).
-record(event_ack, {
  id = <<>> :: binary()
}).

-export([
  start/2,
  stop/1,
  reload/3,
	mod_opt_type/1,
  depends/2,
  store_mam_message/6
]).

start(Host, _Opts) ->
  ejabberd_hooks:add(store_mam_message, Host, ?MODULE, store_mam_message, 91),
  ok.

stop(Host) ->
  ejabberd_hooks:delete(store_mam_message, Host, ?MODULE, store_mam_message, 91),
  ok.

reload(Host, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(Host, NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(Host, OldOpts, ?MODULE),
    if NewMod /= OldMod ->
      NewMod:init(Host, NewOpts);
        true ->
      ok
    end.

mod_opt_type(service_ack) ->
    fun (A) when is_atom(A) -> A end;
mod_opt_type(service_msg) ->
    fun (A) when is_atom(A) -> A end;
mod_opt_type(_) ->
    [service_msg, service_ack].

depends(_,_) ->
	[].

get_service(#event_msg{}, Host) ->
  try_get_option(Host, service_msg, <<"">>);
get_service(#event_ack{}, Host) ->
  try_get_option(Host, service_ack, <<"">>);
get_service(_, Host) ->
  throw({ incorrect_service_kind, ?MODULE, Host }).

try_get_option(Host, OptionName, DefaultValue) ->
    case gen_mod:is_loaded(Host, ?MODULE) of
	true -> ok;
	_ -> throw({module_must_be_started_in_vhost, ?MODULE, Host})
    end,
    gen_mod:get_module_opt(Host, ?MODULE, OptionName, fun(I) -> I end, DefaultValue).

encode(#event_msg{id = Id, from = From, to = To }) -> jiffy:encode(
  {[
    {messageId, Id},
    {senderId, From#jid.user},
    {receiverId, To#jid.user}
  ]}
);
encode(#event_ack{id = Id, from = From, to = To }) -> jiffy:encode(
  {[
    {messageId, Id},
    {senderId, From#jid.user},
    {receiverId, To#jid.user}
  ]}
).

send(Event, Host) ->
  Encoded = encode(Event),
  Service = atom_to_list(get_service(Event, Host)),
  Request = {
    Service,
    [],
    "application/json",
    Encoded
  },
  Response = try httpc:request(post, Request, [], [])
    of R -> R
    catch E -> {error, E}
  end,
  ?INFO_MSG("HTTP response from ~p: ~p", [Service, Response]),
  case Response of
    {error, Reason} -> ?ERROR_MSG("Error in module ~p while sending a request to ~p: ~p", [?MODULE, Service, Reason]), ok;
    _ -> ok
  end.

process_packet(#message{id = Id, from = From, to = To, body = _Body } = Message, Host) ->
  case xmpp:has_subtag(Message, #receipt_response{}) of
    true -> % Receipt of message
      Ack = xmpp:get_subtag(Message, #receipt_response{}),
      AckId = Ack#receipt_response.id,
      send(#event_ack{ id = AckId, from = From, to = To }, Host);
    false -> % New message
%      BodyText = xmpp:get_text(Body),
      send(#event_msg{ id = Id, from = From, to = To }, Host)
  end,
  Message;
process_packet(Pkt, _Host) -> Pkt.


store_mam_message(Pkt, _U, Host, _P, chat, recv) ->
  process_packet(Pkt, Host);
store_mam_message(Pkt, _U, _S, _P, _, _) -> Pkt.
