-xml(inbox_item,
      #elem{name = <<"item">>,
            xmlns = <<"jabber:iq:inbox">>,
            module = xmpp_loveos_inbox,
            result = {inbox_item, '$jid', '$name', '$photo', '$lastmsg', '$read', '$messaged_at' },
            attrs = [
                  #attr{name = <<"jid">>}, 
                  #attr{name = <<"name">>}, 
                  #attr{name = <<"photo">>},
                  #attr{name = <<"lastmsg">>},
                  #attr{name = <<"messaged_at">>},
                  #attr{name = <<"read">>}
            ]}).

-xml(inbox_query,
      #elem{name = <<"query">>,
            xmlns = <<"jabber:iq:inbox">>,
            module = xmpp_loveos_inbox,
            result = {inbox_query, '$items'},
            attrs = [],
            refs = [#ref{name = inbox_item, label = '$items'}]}).
