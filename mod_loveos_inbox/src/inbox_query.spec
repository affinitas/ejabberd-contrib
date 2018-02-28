-xml(inbox_item,
      #elem{name = <<"item">>,
            xmlns = <<"jabber:iq:inbox">>,
            module = xmpp_loveos_inbox,
            result = {inbox_item, '$jid', '$name', '$photo', '$message', '$timestamp', '$read', '$direction' },
            attrs = [
                  #attr{name = <<"jid">>, required = true, dec = {jid, decode, []}, enc = {jid, encode, []}},
                  #attr{name = <<"name">>}, 
                  #attr{name = <<"photo">>},
                  #attr{name = <<"message">>},
                  #attr{name = <<"timestamp">>},
                  #attr{name = <<"read">>},
                  #attr{name = <<"direction">>}
            ]}).

-xml(inbox_query,
      #elem{name = <<"query">>,
            xmlns = <<"jabber:iq:inbox">>,
            module = xmpp_loveos_inbox,
            result = {inbox_query, '$items'},
            attrs = [],
            refs = [#ref{name = inbox_item, label = '$items'}]}).
