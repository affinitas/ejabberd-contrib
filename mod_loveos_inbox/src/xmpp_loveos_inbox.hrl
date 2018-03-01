% This is for new tags to be able to work correctly
% More information at For more information, https://github.com/processone/xmpp/issues/9 

-record(inbox_user, {jid :: jid:jid(),
                     display_name = <<>> :: binary(),
                     picture_url = <<>> :: binary()}).
-type inbox_user() :: #inbox_user{}.

-record(inbox_last_message, {id = <<>> :: binary(),
                             timestamp = <<>> :: binary(),
                             text = <<>> :: binary(),
                             direction = <<>> :: binary(),
                             read = <<>> :: binary()}).
-type inbox_last_message() :: #inbox_last_message{}.

-record(inbox_item, {user :: 'undefined' | #inbox_user{},
                     last_message :: 'undefined' | #inbox_last_message{}}).
-type inbox_item() :: #inbox_item{}.

-record(inbox_query, {items = [] :: [#inbox_item{}]}).
-type inbox_query() :: #inbox_query{}.

-define(NS_INBOX, <<"jabber:iq:inbox">>).
