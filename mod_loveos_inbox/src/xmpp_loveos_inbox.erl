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
do_decode(<<"last-message">>, <<"jabber:iq:inbox">>, El,
	  Opts) ->
    decode_inbox_last_message(<<"jabber:iq:inbox">>, Opts,
			      El);
do_decode(<<"user">>, <<"jabber:iq:inbox">>, El,
	  Opts) ->
    decode_inbox_user(<<"jabber:iq:inbox">>, Opts, El);
do_decode(Name, <<>>, _, _) ->
    erlang:error({xmpp_codec, {missing_tag_xmlns, Name}});
do_decode(Name, XMLNS, _, _) ->
    erlang:error({xmpp_codec, {unknown_tag, Name, XMLNS}}).

tags() ->
    [{<<"query">>, <<"jabber:iq:inbox">>},
     {<<"item">>, <<"jabber:iq:inbox">>},
     {<<"last-message">>, <<"jabber:iq:inbox">>},
     {<<"user">>, <<"jabber:iq:inbox">>}].

do_encode({inbox_user, _, _, _} = User, TopXMLNS) ->
    encode_inbox_user(User, TopXMLNS);
do_encode({inbox_last_message, _, _, _, _, _} =
	      Last_message,
	  TopXMLNS) ->
    encode_inbox_last_message(Last_message, TopXMLNS);
do_encode({inbox_item, _, _} = Item, TopXMLNS) ->
    encode_inbox_item(Item, TopXMLNS);
do_encode({inbox_query, _} = Query, TopXMLNS) ->
    encode_inbox_query(Query, TopXMLNS).

do_get_name({inbox_item, _, _}) -> <<"item">>;
do_get_name({inbox_last_message, _, _, _, _, _}) ->
    <<"last-message">>;
do_get_name({inbox_query, _}) -> <<"query">>;
do_get_name({inbox_user, _, _, _}) -> <<"user">>.

do_get_ns({inbox_item, _, _}) -> <<"jabber:iq:inbox">>;
do_get_ns({inbox_last_message, _, _, _, _, _}) ->
    <<"jabber:iq:inbox">>;
do_get_ns({inbox_query, _}) -> <<"jabber:iq:inbox">>;
do_get_ns({inbox_user, _, _, _}) ->
    <<"jabber:iq:inbox">>.

pp(inbox_user, 3) -> [jid, display_name, picture_url];
pp(inbox_last_message, 5) ->
    [id, timestamp, text, direction, read];
pp(inbox_item, 2) -> [user, last_message];
pp(inbox_query, 1) -> [items];
pp(_, _) -> no.

records() ->
    [{inbox_user, 3}, {inbox_last_message, 5},
     {inbox_item, 2}, {inbox_query, 1}].

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
    {User, Last_message} = decode_inbox_item_els(__TopXMLNS,
						 __Opts, _els, undefined,
						 undefined),
    {inbox_item, User, Last_message}.

decode_inbox_item_els(__TopXMLNS, __Opts, [], User,
		      Last_message) ->
    {User, Last_message};
decode_inbox_item_els(__TopXMLNS, __Opts,
		      [{xmlel, <<"user">>, _attrs, _} = _el | _els], User,
		      Last_message) ->
    case xmpp_codec:get_attr(<<"xmlns">>, _attrs,
			     __TopXMLNS)
	of
      <<"jabber:iq:inbox">> ->
	  decode_inbox_item_els(__TopXMLNS, __Opts, _els,
				decode_inbox_user(<<"jabber:iq:inbox">>, __Opts,
						  _el),
				Last_message);
      _ ->
	  decode_inbox_item_els(__TopXMLNS, __Opts, _els, User,
				Last_message)
    end;
decode_inbox_item_els(__TopXMLNS, __Opts,
		      [{xmlel, <<"last-message">>, _attrs, _} = _el | _els],
		      User, Last_message) ->
    case xmpp_codec:get_attr(<<"xmlns">>, _attrs,
			     __TopXMLNS)
	of
      <<"jabber:iq:inbox">> ->
	  decode_inbox_item_els(__TopXMLNS, __Opts, _els, User,
				decode_inbox_last_message(<<"jabber:iq:inbox">>,
							  __Opts, _el));
      _ ->
	  decode_inbox_item_els(__TopXMLNS, __Opts, _els, User,
				Last_message)
    end;
decode_inbox_item_els(__TopXMLNS, __Opts, [_ | _els],
		      User, Last_message) ->
    decode_inbox_item_els(__TopXMLNS, __Opts, _els, User,
			  Last_message).

encode_inbox_item({inbox_item, User, Last_message},
		  __TopXMLNS) ->
    __NewTopXMLNS =
	xmpp_codec:choose_top_xmlns(<<"jabber:iq:inbox">>, [],
				    __TopXMLNS),
    _els = lists:reverse('encode_inbox_item_$user'(User,
						   __NewTopXMLNS,
						   'encode_inbox_item_$last_message'(Last_message,
										     __NewTopXMLNS,
										     []))),
    _attrs = xmpp_codec:enc_xmlns_attrs(__NewTopXMLNS,
					__TopXMLNS),
    {xmlel, <<"item">>, _attrs, _els}.

'encode_inbox_item_$user'(undefined, __TopXMLNS,
			  _acc) ->
    _acc;
'encode_inbox_item_$user'(User, __TopXMLNS, _acc) ->
    [encode_inbox_user(User, __TopXMLNS) | _acc].

'encode_inbox_item_$last_message'(undefined, __TopXMLNS,
				  _acc) ->
    _acc;
'encode_inbox_item_$last_message'(Last_message,
				  __TopXMLNS, _acc) ->
    [encode_inbox_last_message(Last_message, __TopXMLNS)
     | _acc].

decode_inbox_last_message(__TopXMLNS, __Opts,
			  {xmlel, <<"last-message">>, _attrs, _els}) ->
    {Id, Timestamp, Text, Direction, Read} =
	decode_inbox_last_message_attrs(__TopXMLNS, _attrs,
					undefined, undefined, undefined,
					undefined, undefined),
    {inbox_last_message, Id, Timestamp, Text, Direction,
     Read}.

decode_inbox_last_message_attrs(__TopXMLNS,
				[{<<"id">>, _val} | _attrs], _Id, Timestamp,
				Text, Direction, Read) ->
    decode_inbox_last_message_attrs(__TopXMLNS, _attrs,
				    _val, Timestamp, Text, Direction, Read);
decode_inbox_last_message_attrs(__TopXMLNS,
				[{<<"timestamp">>, _val} | _attrs], Id,
				_Timestamp, Text, Direction, Read) ->
    decode_inbox_last_message_attrs(__TopXMLNS, _attrs, Id,
				    _val, Text, Direction, Read);
decode_inbox_last_message_attrs(__TopXMLNS,
				[{<<"text">>, _val} | _attrs], Id, Timestamp,
				_Text, Direction, Read) ->
    decode_inbox_last_message_attrs(__TopXMLNS, _attrs, Id,
				    Timestamp, _val, Direction, Read);
decode_inbox_last_message_attrs(__TopXMLNS,
				[{<<"direction">>, _val} | _attrs], Id,
				Timestamp, Text, _Direction, Read) ->
    decode_inbox_last_message_attrs(__TopXMLNS, _attrs, Id,
				    Timestamp, Text, _val, Read);
decode_inbox_last_message_attrs(__TopXMLNS,
				[{<<"read">>, _val} | _attrs], Id, Timestamp,
				Text, Direction, _Read) ->
    decode_inbox_last_message_attrs(__TopXMLNS, _attrs, Id,
				    Timestamp, Text, Direction, _val);
decode_inbox_last_message_attrs(__TopXMLNS,
				[_ | _attrs], Id, Timestamp, Text, Direction,
				Read) ->
    decode_inbox_last_message_attrs(__TopXMLNS, _attrs, Id,
				    Timestamp, Text, Direction, Read);
decode_inbox_last_message_attrs(__TopXMLNS, [], Id,
				Timestamp, Text, Direction, Read) ->
    {decode_inbox_last_message_attr_id(__TopXMLNS, Id),
     decode_inbox_last_message_attr_timestamp(__TopXMLNS,
					      Timestamp),
     decode_inbox_last_message_attr_text(__TopXMLNS, Text),
     decode_inbox_last_message_attr_direction(__TopXMLNS,
					      Direction),
     decode_inbox_last_message_attr_read(__TopXMLNS, Read)}.

encode_inbox_last_message({inbox_last_message, Id,
			   Timestamp, Text, Direction, Read},
			  __TopXMLNS) ->
    __NewTopXMLNS =
	xmpp_codec:choose_top_xmlns(<<"jabber:iq:inbox">>, [],
				    __TopXMLNS),
    _els = [],
    _attrs = encode_inbox_last_message_attr_read(Read,
						 encode_inbox_last_message_attr_direction(Direction,
											  encode_inbox_last_message_attr_text(Text,
															      encode_inbox_last_message_attr_timestamp(Timestamp,
																				       encode_inbox_last_message_attr_id(Id,
																									 xmpp_codec:enc_xmlns_attrs(__NewTopXMLNS,
																												    __TopXMLNS)))))),
    {xmlel, <<"last-message">>, _attrs, _els}.

decode_inbox_last_message_attr_id(__TopXMLNS,
				  undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"id">>, <<"last-message">>,
		   __TopXMLNS}});
decode_inbox_last_message_attr_id(__TopXMLNS, _val) ->
    _val.

encode_inbox_last_message_attr_id(_val, _acc) ->
    [{<<"id">>, _val} | _acc].

decode_inbox_last_message_attr_timestamp(__TopXMLNS,
					 undefined) ->
    <<>>;
decode_inbox_last_message_attr_timestamp(__TopXMLNS,
					 _val) ->
    _val.

encode_inbox_last_message_attr_timestamp(<<>>, _acc) ->
    _acc;
encode_inbox_last_message_attr_timestamp(_val, _acc) ->
    [{<<"timestamp">>, _val} | _acc].

decode_inbox_last_message_attr_text(__TopXMLNS,
				    undefined) ->
    <<>>;
decode_inbox_last_message_attr_text(__TopXMLNS, _val) ->
    _val.

encode_inbox_last_message_attr_text(<<>>, _acc) -> _acc;
encode_inbox_last_message_attr_text(_val, _acc) ->
    [{<<"text">>, _val} | _acc].

decode_inbox_last_message_attr_direction(__TopXMLNS,
					 undefined) ->
    <<>>;
decode_inbox_last_message_attr_direction(__TopXMLNS,
					 _val) ->
    _val.

encode_inbox_last_message_attr_direction(<<>>, _acc) ->
    _acc;
encode_inbox_last_message_attr_direction(_val, _acc) ->
    [{<<"direction">>, _val} | _acc].

decode_inbox_last_message_attr_read(__TopXMLNS,
				    undefined) ->
    <<>>;
decode_inbox_last_message_attr_read(__TopXMLNS, _val) ->
    _val.

encode_inbox_last_message_attr_read(<<>>, _acc) -> _acc;
encode_inbox_last_message_attr_read(_val, _acc) ->
    [{<<"read">>, _val} | _acc].

decode_inbox_user(__TopXMLNS, __Opts,
		  {xmlel, <<"user">>, _attrs, _els}) ->
    {Jid, Display_name, Picture_url} =
	decode_inbox_user_attrs(__TopXMLNS, _attrs, undefined,
				undefined, undefined),
    {inbox_user, Jid, Display_name, Picture_url}.

decode_inbox_user_attrs(__TopXMLNS,
			[{<<"jid">>, _val} | _attrs], _Jid, Display_name,
			Picture_url) ->
    decode_inbox_user_attrs(__TopXMLNS, _attrs, _val,
			    Display_name, Picture_url);
decode_inbox_user_attrs(__TopXMLNS,
			[{<<"display-name">>, _val} | _attrs], Jid,
			_Display_name, Picture_url) ->
    decode_inbox_user_attrs(__TopXMLNS, _attrs, Jid, _val,
			    Picture_url);
decode_inbox_user_attrs(__TopXMLNS,
			[{<<"picture-url">>, _val} | _attrs], Jid, Display_name,
			_Picture_url) ->
    decode_inbox_user_attrs(__TopXMLNS, _attrs, Jid,
			    Display_name, _val);
decode_inbox_user_attrs(__TopXMLNS, [_ | _attrs], Jid,
			Display_name, Picture_url) ->
    decode_inbox_user_attrs(__TopXMLNS, _attrs, Jid,
			    Display_name, Picture_url);
decode_inbox_user_attrs(__TopXMLNS, [], Jid,
			Display_name, Picture_url) ->
    {decode_inbox_user_attr_jid(__TopXMLNS, Jid),
     'decode_inbox_user_attr_display-name'(__TopXMLNS,
					   Display_name),
     'decode_inbox_user_attr_picture-url'(__TopXMLNS,
					  Picture_url)}.

encode_inbox_user({inbox_user, Jid, Display_name,
		   Picture_url},
		  __TopXMLNS) ->
    __NewTopXMLNS =
	xmpp_codec:choose_top_xmlns(<<"jabber:iq:inbox">>, [],
				    __TopXMLNS),
    _els = [],
    _attrs =
	'encode_inbox_user_attr_picture-url'(Picture_url,
					     'encode_inbox_user_attr_display-name'(Display_name,
										   encode_inbox_user_attr_jid(Jid,
													      xmpp_codec:enc_xmlns_attrs(__NewTopXMLNS,
																	 __TopXMLNS)))),
    {xmlel, <<"user">>, _attrs, _els}.

decode_inbox_user_attr_jid(__TopXMLNS, undefined) ->
    erlang:error({xmpp_codec,
		  {missing_attr, <<"jid">>, <<"user">>, __TopXMLNS}});
decode_inbox_user_attr_jid(__TopXMLNS, _val) ->
    case catch jid:decode(_val) of
      {'EXIT', _} ->
	  erlang:error({xmpp_codec,
			{bad_attr_value, <<"jid">>, <<"user">>, __TopXMLNS}});
      _res -> _res
    end.

encode_inbox_user_attr_jid(_val, _acc) ->
    [{<<"jid">>, jid:encode(_val)} | _acc].

'decode_inbox_user_attr_display-name'(__TopXMLNS,
				      undefined) ->
    <<>>;
'decode_inbox_user_attr_display-name'(__TopXMLNS,
				      _val) ->
    _val.

'encode_inbox_user_attr_display-name'(<<>>, _acc) ->
    _acc;
'encode_inbox_user_attr_display-name'(_val, _acc) ->
    [{<<"display-name">>, _val} | _acc].

'decode_inbox_user_attr_picture-url'(__TopXMLNS,
				     undefined) ->
    <<>>;
'decode_inbox_user_attr_picture-url'(__TopXMLNS,
				     _val) ->
    _val.

'encode_inbox_user_attr_picture-url'(<<>>, _acc) ->
    _acc;
'encode_inbox_user_attr_picture-url'(_val, _acc) ->
    [{<<"picture-url">>, _val} | _acc].
