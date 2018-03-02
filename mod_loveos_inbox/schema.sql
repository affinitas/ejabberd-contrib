CREATE TABLE loveos_inbox_v1 (
  id text not null,
  username text not null,
  peer text not null,
  message text,
  direction char not null,
  read boolean not null,
  timestamp timestamp default now()
);

CREATE INDEX i_loveos_inbox_v1 ON loveos_inbox_v1 using btree (username);
CREATE UNIQUE INDEX i_loveos_inbox_v1_user_peer ON loveos_inbox_v1 using btree (username, peer);
alter table loveos_inbox_v1 add constraint c_loveos_inbox_v1_user_peer unique(username, peer);