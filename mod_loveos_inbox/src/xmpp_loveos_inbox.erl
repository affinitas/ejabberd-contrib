%% Created automatically by XML generator (fxml_gen.erl)
%% Source: xmpp_codec.spec

-module(xmpp_loveos_inbox).

-compile(export_all).

do_decode(<<"query">>, <<"jabber:iq:inbox">>, El,
	  Opts) ->
    decode_inbox_query(<<"jabber:iq:inbox">>, Opts, El);
do_decode(<<"item">>, <<"jabber:iq:inbox">>, El,
	  Opts) ->
    decode_inbox_item(<<"jabber:iq:inbox">>, Opts, El);
do_decode(Name, <<>>, _, _) ->
    erlang:error({xmpp_codec, {missing_tag_xmlns, Name}});
do_decode(Name, XMLNS, _, _) ->
    erlang:error({xmpp_codec, {unknown_tag, Name, XMLNS}}).

tags() ->
    [{<<"query">>, <<"jabber:iq:inbox">>},
     {<<"item">>, <<"jabber:iq:inbox">>}].

do_encode({inbox_item, _, _, _, _, _, _} = Item,
	  TopXMLNS) ->
    encode_inbox_item(Item, TopXMLNS);
do_encode({inbox_query, _} = Query, TopXMLNS) ->
    encode_inbox_query(Query, TopXMLNS).

do_get_name({inbox_item, _, _, _, _, _, _}) ->
    <<"item">>;
do_get_name({inbox_query, _}) -> <<"query">>.

do_get_ns({inbox_item, _, _, _, _, _, _}) ->
    <<"jabber:iq:inbox">>;
do_get_ns({inbox_query, _}) -> <<"jabber:iq:inbox">>.

pp(inbox_item, 6) ->
    [jid, name, photo, lastmsg, read, messaged_at];
pp(inbox_query, 1) -> [items];
pp(_, _) -> no.

records() -> [{inbox_item, 6}, {inbox_query, 1}].

decode_inbox_query(__TopXMLNS, __Opts,
		   {xmlel, <<"query">>, _attrs, _els}) ->
    Items = decode_inbox_query_els(__TopXMLNS, __Opts, _els,
				   []),
    {inbox_query, Items}.

decode_inbox_query_els(__TopXMLNS, __Opts, [], Items) ->
    lists:reverse(Items);
decode_inbox_query_els(__TopXMLNS, __Opts,
		       [{xmlel, <<"item">>, _attrs, _} = _el | _els], Items) ->
    case xmpp_codec:get_attr(<<"xmlns">>, _attrs,
			     __TopXMLNS)
	of
      <<"jabber:iq:inbox">> ->
	  decode_inbox_query_els(__TopXMLNS, __Opts, _els,
				 [decode_inbox_item(<<"jabber:iq:inbox">>,
						    __Opts, _el)
				  | Items]);
      _ ->
	  decode_inbox_query_els(__TopXMLNS, __Opts, _els, Items)
    end;
decode_inbox_query_els(__TopXMLNS, __Opts, [_ | _els],
		       Items) ->
    decode_inbox_query_els(__TopXMLNS, __Opts, _els, Items).

encode_inbox_query({inbox_query, Items}, __TopXMLNS) ->
    __NewTopXMLNS =
	xmpp_codec:choose_top_xmlns(<<"jabber:iq:inbox">>, [],
				    __TopXMLNS),
    _els = lists:reverse('encode_inbox_query_$items'(Items,
						     __NewTopXMLNS, [])),
    _attrs = xmpp_codec:enc_xmlns_attrs(__NewTopXMLNS,
					__TopXMLNS),
    {xmlel, <<"query">>, _attrs, _els}.

'encode_inbox_query_$items'([], __TopXMLNS, _acc) ->
    _acc;
'encode_inbox_query_$items'([Items | _els], __TopXMLNS,
			    _acc) ->
    'encode_inbox_query_$items'(_els, __TopXMLNS,
				[encode_inbox_item(Items, __TopXMLNS) | _acc]).

decode_inbox_item(__TopXMLNS, __Opts,
		  {xmlel, <<"item">>, _attrs, _els}) ->
    {Jid, Name, Photo, Lastmsg, Messaged_at, Read} =
	decode_inbox_item_attrs(__TopXMLNS, _attrs, undefined,
				undefined, undefined, undefined, undefined,
				undefined),
    {inbox_item, Jid, Name, Photo, Lastmsg, Read,
     Messaged_at}.

decode_inbox_item_attrs(__TopXMLNS,
			[{<<"jid">>, _val} | _attrs], _Jid, Name, Photo,
			Lastmsg, Messaged_at, Read) ->
    decode_inbox_item_attrs(__TopXMLNS, _attrs, _val, Name,
			    Photo, Lastmsg, Messaged_at, Read);
decode_inbox_item_attrs(__TopXMLNS,
			[{<<"name">>, _val} | _attrs], Jid, _Name, Photo,
			Lastmsg, Messaged_at, Read) ->
    decode_inbox_item_attrs(__TopXMLNS, _attrs, Jid, _val,
			    Photo, Lastmsg, Messaged_at, Read);
decode_inbox_item_attrs(__TopXMLNS,
			[{<<"photo">>, _val} | _attrs], Jid, Name, _Photo,
			Lastmsg, Messaged_at, Read) ->
    decode_inbox_item_attrs(__TopXMLNS, _attrs, Jid, Name,
			    _val, Lastmsg, Messaged_at, Read);
decode_inbox_item_attrs(__TopXMLNS,
			[{<<"lastmsg">>, _val} | _attrs], Jid, Name, Photo,
			_Lastmsg, Messaged_at, Read) ->
    decode_inbox_item_attrs(__TopXMLNS, _attrs, Jid, Name,
			    Photo, _val, Messaged_at, Read);
decode_inbox_item_attrs(__TopXMLNS,
			[{<<"messaged_at">>, _val} | _attrs], Jid, Name, Photo,
			Lastmsg, _Messaged_at, Read) ->
    decode_inbox_item_attrs(__TopXMLNS, _attrs, Jid, Name,
			    Photo, Lastmsg, _val, Read);
decode_inbox_item_attrs(__TopXMLNS,
			[{<<"read">>, _val} | _attrs], Jid, Name, Photo,
			Lastmsg, Messaged_at, _Read) ->
    decode_inbox_item_attrs(__TopXMLNS, _attrs, Jid, Name,
			    Photo, Lastmsg, Messaged_at, _val);
decode_inbox_item_attrs(__TopXMLNS, [_ | _attrs], Jid,
			Name, Photo, Lastmsg, Messaged_at, Read) ->
    decode_inbox_item_attrs(__TopXMLNS, _attrs, Jid, Name,
			    Photo, Lastmsg, Messaged_at, Read);
decode_inbox_item_attrs(__TopXMLNS, [], Jid, Name,
			Photo, Lastmsg, Messaged_at, Read) ->
    {decode_inbox_item_attr_jid(__TopXMLNS, Jid),
     decode_inbox_item_attr_name(__TopXMLNS, Name),
     decode_inbox_item_attr_photo(__TopXMLNS, Photo),
     decode_inbox_item_attr_lastmsg(__TopXMLNS, Lastmsg),
     decode_inbox_item_attr_messaged_at(__TopXMLNS,
					Messaged_at),
     decode_inbox_item_attr_read(__TopXMLNS, Read)}.

encode_inbox_item({inbox_item, Jid, Name, Photo,
		   Lastmsg, Read, Messaged_at},
		  __TopXMLNS) ->
    __NewTopXMLNS =
	xmpp_codec:choose_top_xmlns(<<"jabber:iq:inbox">>, [],
				    __TopXMLNS),
    _els = [],
    _attrs = encode_inbox_item_attr_read(Read,
					 encode_inbox_item_attr_messaged_at(Messaged_at,
									    encode_inbox_item_attr_lastmsg(Lastmsg,
													   encode_inbox_item_attr_photo(Photo,
																	encode_inbox_item_attr_name(Name,
																				    encode_inbox_item_attr_jid(Jid,
																							       xmpp_codec:enc_xmlns_attrs(__NewTopXMLNS,
																											  __TopXMLNS))))))),
    {xmlel, <<"item">>, _attrs, _els}.

decode_inbox_item_attr_jid(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"item">>, __TopXMLNS}});
decode_inbox_item_attr_jid(__TopXMLNS, _val) ->
    case catch jid:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"item">>, __TopXMLNS}});
      _res -> _res
    end.

encode_inbox_item_attr_jid(_val, _acc) ->
    [{<<"jid">>, jid:encode(_val)} | _acc].

decode_inbox_item_attr_name(__TopXMLNS, undefined) ->
    <<>>;
decode_inbox_item_attr_name(__TopXMLNS, _val) -> _val.

encode_inbox_item_attr_name(<<>>, _acc) -> _acc;
encode_inbox_item_attr_name(_val, _acc) ->
    [{<<"name">>, _val} | _acc].

decode_inbox_item_attr_photo(__TopXMLNS, undefined) ->
    <<>>;
decode_inbox_item_attr_photo(__TopXMLNS, _val) -> _val.

encode_inbox_item_attr_photo(<<>>, _acc) -> _acc;
encode_inbox_item_attr_photo(_val, _acc) ->
    [{<<"photo">>, _val} | _acc].

decode_inbox_item_attr_lastmsg(__TopXMLNS, undefined) ->
    <<>>;
decode_inbox_item_attr_lastmsg(__TopXMLNS, _val) ->
    _val.

encode_inbox_item_attr_lastmsg(<<>>, _acc) -> _acc;
encode_inbox_item_attr_lastmsg(_val, _acc) ->
    [{<<"lastmsg">>, _val} | _acc].

decode_inbox_item_attr_messaged_at(__TopXMLNS,
				   undefined) ->
    <<>>;
decode_inbox_item_attr_messaged_at(__TopXMLNS, _val) ->
    _val.

encode_inbox_item_attr_messaged_at(<<>>, _acc) -> _acc;
encode_inbox_item_attr_messaged_at(_val, _acc) ->
    [{<<"messaged_at">>, _val} | _acc].

decode_inbox_item_attr_read(__TopXMLNS, undefined) ->
    <<>>;
decode_inbox_item_attr_read(__TopXMLNS, _val) -> _val.

encode_inbox_item_attr_read(<<>>, _acc) -> _acc;
encode_inbox_item_attr_read(_val, _acc) ->
    [{<<"read">>, _val} | _acc].