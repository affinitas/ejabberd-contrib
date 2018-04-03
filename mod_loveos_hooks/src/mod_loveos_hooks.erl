%%%-------------------------------------------------------------------
%%% @author Igor Dultsev
%%% ©copyright (C) 2017-2018, Spark Networks Services, GmbH
%%% @doc
%%% Returns inbox to users
%%% @end
%%%-------------------------------------------------------------------

-module(mod_loveos_hooks).
-author("Igor Dultsev").

-behavior(gen_mod).
-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("logger.hrl").
-include("mod_mam.hrl").

-export([start/2, stop/1, depends/2, reload/3 ]).
-export([sm_receive_packet/1, user_receive_packet/1, user_send_packet/1, offline_message/1 ]).


%%% Module start/stop

start(Host, Opts) ->
  IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1, one_queue),             %% IQDisc is required for module to IQ handler to work
  ejabberd_hooks:add(sm_receive_packet, Host, ?MODULE, sm_receive_packet, 50),
  ejabberd_hooks:add(user_receive_packet, Host, ?MODULE, user_receive_packet, 88),
  ejabberd_hooks:add(user_send_packet, Host, ?MODULE, user_send_packet, 88),
  ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, offline_message, 50),
  ok.

stop(Host) ->
  ejabberd_hooks:delete(sm_receive_packet, Host, ?MODULE, sm_receive_packet, 50),
  ejabberd_hooks:delete(user_receive_packet, Host, ?MODULE, user_receive_packet, 88),
  ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, user_send_packet, 88),
  ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, offline_message, 50),
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

sm_receive_packet(Packet) -> 
  ?INFO_MSG("sm_receive_packet", []),
  Packet.

user_receive_packet(Packet) -> 
  ?INFO_MSG("user_receive_packet", []),
  Packet.

user_send_packet(Packet) -> 
  ?INFO_MSG("user_send_packet", []),
  Packet.

offline_message(Packet) -> 
  ?INFO_MSG("offline_message_hook", []),
  Packet.


