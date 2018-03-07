CREATE TABLE loveos_inbox_v1 (
  id text not null,
  username text not null,
  peer_user text not null,
  peer_server text not null,
  message text,
  direction char not null,
  read boolean not null,
  timestamp timestamp with time zone default now()
);

CREATE INDEX i_loveos_inbox_v1 ON loveos_inbox_v1 using btree (username);
alter table loveos_inbox_v1 add constraint c_loveos_inbox_v1_user_peer unique(username, peer_user, peer_server);
