CREATE TABLE loveos_inbox (
  id text not null,
  username text not null,
  peer_user text not null,
  peer_server text not null,
  message text,
  direction char not null,
  read boolean not null,
  timestamp timestamp with time zone default now()
);

CREATE INDEX i_loveos_inbox ON loveos_inbox using btree (username);
alter table loveos_inbox add constraint c_loveos_inbox_user_peer unique(username, peer_user, peer_server);
