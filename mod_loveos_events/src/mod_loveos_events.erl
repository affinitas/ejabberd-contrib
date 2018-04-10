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
  ?INFO_MSG("Starting with service: ~p", [get_service(Host)]),
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

mod_opt_type(service) ->
    fun (A) when is_atom(A) -> A end;

mod_opt_type(_) ->
    [service].

depends(_,_) ->
	[].

get_service(Host) -> 
  try_get_option(Host, service, <<"">>).

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
encode(#event_ack{id = Id}) -> jiffy:encode(
  {[
    {messageId, Id}
  ]} 
).

send(Event) -> 
  ?INFO_MSG("Send JSON: ~p", [encode(Event)]),
  ok.

process_packet(#message{id = Id, from = From, to = To, body = _Body } = Message) ->
  case xmpp:has_subtag(Message, #receipt_response{}) of
    true ->
      Ack = xmpp:get_subtag(Message, #receipt_response{}),
      AckId = Ack#message.id,
      send(#event_ack{id = AckId});
    false -> 
%      BodyText = xmpp:get_text(Body),
      send(#event_msg{ id = Id, from = From, to = To })
  end,
  Message;
process_packet(Pkt) -> Pkt.


store_mam_message(Pkt, _U, _S, _P, chat, recv) -> 
  process_packet(Pkt);
store_mam_message(Pkt, _U, _S, _P, _, _) -> Pkt.