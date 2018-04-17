
-xml(inbox_user,
      #elem{name = <<"user">>,
      xmlns = <<"jabber:iq:inbox">>,
      module = xmpp_loveos_inbox,
      result = { inbox_user, '$jid', '$display_name', '$picture_url' },
      attrs = [
            #attr{name = <<"jid">>, required = true, dec = {jid, decode, []}, enc = {jid, encode, []}},
            #attr{name = <<"display-name">>, label='$display_name'},
            #attr{name = <<"picture-url">>, label='$picture_url'}
      ]}).

-xml(inbox_last_message,
      #elem{name = <<"last-message">>,
      xmlns = <<"jabber:iq:inbox">>,
      module = xmpp_loveos_inbox,
      result = { inbox_last_message, '$id', '$timestamp', '$text', '$direction', '$read' },
      attrs = [
            #attr{name = <<"id">>, required = true},
            #attr{name = <<"timestamp">>},
            #attr{name = <<"text">>},
            #attr{name = <<"direction">>},
            #attr{name = <<"read">>}
      ]}).

-xml(inbox_item,
      #elem{name = <<"item">>,
            xmlns = <<"jabber:iq:inbox">>,
            module = xmpp_loveos_inbox,
            result = {inbox_item, '$user', '$last_message' },
            attrs = [],
            refs = [
                  #ref{name = inbox_user, min = 0, max = 1, label = '$user' },
                  #ref{name = inbox_last_message, min = 0, max = 1, label = '$last_message' }
            ]}).

-xml(inbox_query,
      #elem{name = <<"query">>,
            xmlns = <<"jabber:iq:inbox">>,
            module = xmpp_loveos_inbox,
            result = {inbox_query, '$items'},
            attrs = [],
            refs = [#ref{name = inbox_item, label = '$items'}]}).
