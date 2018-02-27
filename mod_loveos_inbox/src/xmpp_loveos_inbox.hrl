% This is for new tags to be able to work correctly
% More information at For more information, https://github.com/processone/xmpp/issues/9 

-record(inbox_item, {jid :: jid:jid(),
                     name = <<>> :: binary(),
                     photo = <<>> :: binary(),
                     lastmsg = <<>> :: binary(),
                     read = <<>> :: binary(),
                     messaged_at = <<>> :: binary()}).
-type inbox_item() :: #inbox_item{}.

-record(inbox_query, {items = [] :: [#inbox_item{}]}).
-type inbox_query() :: #inbox_query{}.

-define(NS_INBOX, <<"jabber:iq:inbox">>).
