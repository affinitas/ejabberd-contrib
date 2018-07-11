%%%-------------------------------------------------------------------
%%% @author Igor Dultsev
%%% ©copyright (C) 2017-2018, Affinitas.de
%%% @doc
%%%
%%%
%%% @end
%%% Create: 6th of July 2018
%%%-------------------------------------------------------------------

-module(mod_loveos_wave).
-author("Igor Dultsev").

-behavior(gen_mod).
-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_commands.hrl").
-include("ejabberd_sql_pt.hrl").
-include("mod_mam.hrl").
-include("ejabberd_http.hrl").
-include("ejabberd_ctl.hrl").

-export([
  start/2,
  stop/1,
  depends/2,
	process/2,
  mod_opt_type/1, 
  mod_options/1,
  get_jid_host/2,
  get_jid/3,
  post_wave/2,
  try_post_wave/2
]).

start(_Host, _Opts) -> 
  ?INFO_MSG("Starting with ~p", _Opts),
  ok.
stop(_Host) -> ok.
depends(_Host, _Opts) -> [].

try_get_option(Host, OptionName, DefaultValue) ->
    case gen_mod:is_loaded(Host, ?MODULE) of
	true -> ok;
	_ -> throw({module_must_be_started_in_vhost, ?MODULE, Host})
    end,
    gen_mod:get_module_opt(Host, ?MODULE, OptionName, fun(I) -> I end, DefaultValue).

process([], #request{method = 'POST', data = Data, host = Host}) ->
  try
    Decoded = jiffy:decode(Data),
    case Decoded of
      {[
	{<<"messageId">>, MessageId},
	{<<"sender">>, {[
	  {<<"userId">>, SenderId}, 
	  {<<"brandId">>, SenderBrand}
        ]}},
        {<<"receiver">>,{[
	  {<<"userId">>, ReceiverId}, 
	  {<<"brandId">>, ReceiverBrand}
	]}}
      ]} ->
          From = get_jid(Host, SenderId, SenderBrand),
          To = get_jid(Host, ReceiverId, ReceiverBrand),
          try_post_wave(MessageId, From, To);
      Anything ->
        ?INFO_MSG("Cannot decode data: ~p", [Anything]),
        {400, [], <<"incorrect_request">>}
    end
  catch
    Error ->
      ?ERROR_MSG("Cannot decode data: ~p ~p", [Data, Error]),
      {500, [], <<"internal_error">>}
  end.

mod_opt_type(brands) -> fun (A) -> A end.
mod_options(_Host) -> [{brands, []}].


get_jid_host(Host, Brand) -> 
  Brands = try_get_option(Host, brands, []),
  Result = [ X || X <- Brands, case X of [{name, Brand}, _Jid] -> true; _ -> false end],
  case Result of
    [[_Brand, {jid_host, JidHost}]] -> JidHost;
    _Error -> error
  end.

get_jid(Host, User, Brand) ->
  UserHost = get_jid_host(Host, Brand),
  ?INFO_MSG("User: ~p @ ~p [~p]", [User, UserHost, Brand]),
  jid:make(User, UserHost).

try_post_wave(Id, From, To) ->
  case From of
    error -> {404, [], <<"incorrect_sender_brand_id">>};
    _ -> case To of
      error -> {404, [], <<"incorrect_receiver_brand_id">>};
      _ -> post_wave(Id, From, To)
    end
  end.

post_wave(Id, From, To) ->
  Pkt = #message{
    id = Id,
    type = chat, 
    from = From, 
    to = To,
    body = xmpp:mk_text(<<238,144,158>>)
  },
  ?INFO_MSG("PKT: ~p", [Pkt]),
  try
    mod_mam:user_send_packet({Pkt, #{jid => xmpp:get_from(Pkt)}}),
    case ejabberd_router:route(Pkt) of
      ok -> {201, [], <<"">>};
      _ -> {400, [], <<"incorrect_request">>}
    end
  catch
    Error ->
      ?ERROR_MSG("Error ~p", [Error]),
      {500, [], <<"internal_error">>}
  end.
